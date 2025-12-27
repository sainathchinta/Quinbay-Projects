package com.gdn.mta.bulk.service;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkProcessPath;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.entity.ProductCodeAndCategoryCodeEvent;
import com.gdn.mta.bulk.entity.ProductCodeAndCategoryCodeListEvent;
import com.gdn.mta.bulk.entity.ProductRecatStatus;
import com.gdn.mta.bulk.entity.RecatProcess;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.mta.bulk.util.ResponseHelper;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.RecatConstants;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class RecatProcessServiceWrapperImpl implements RecatProcessServiceWrapper {

  private static final String RECAT = "recat";
  @Autowired
  private RecatProcessService recatProcessService;

  @Autowired
  private ProductRecatStatusService productRecatStatusService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Autowired
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Async
  @Trace(dispatcher = true)
  @Override
  public void processNewRecatProcess(String storeId) {
    try {
      SystemParameterConfig processNewRequest =
          systemParameterConfigService.findValueByStoreIdAndVariable(storeId, RecatConstants.PROCESS_NEW_REQUESTS);
      if (Boolean.valueOf(processNewRequest.getValue())) {
        List<RecatProcess> recatProcessList =
            recatProcessService.getAllEligibleNewRecatProcess(storeId, RecatConstants.NEW);
        if (CollectionUtils.isNotEmpty(recatProcessList)) {
          SystemParameterConfig recatBatchSizeConfig =
            systemParameterConfigService.findValueByStoreIdAndVariable(storeId, RecatConstants.RECAT_BATCH_SIZE);
          List<RecatProcess> batchedRecatProcesses =
            recatProcessList.stream().limit(Long.parseLong(recatBatchSizeConfig.getValue())).collect(
              Collectors.toList());
          for (RecatProcess recatProcess : batchedRecatProcesses)
            processRecatFile(storeId, recatProcess);
        }
      }
    } catch (Exception e) {
      log.error("Error when processing new recat requests, error -  ", e);
    }
  }


  public void processRecatFile(String storeId, RecatProcess recatProcess) {
    try {
      log.info("Starting pre-process of Recat-request-code : {} ", recatProcess.getRecatRequestCode());
      Collection<ProductRecatStatus> productRecatStatusList = getProductRecatDataFromExcel(
        recatProcess);
      saveProductRecatData(storeId, productRecatStatusList);
      log.info("Products data stored for Recat-request-code : {} ", recatProcess.getRecatRequestCode());
      recatProcess.setTotalCount(productRecatStatusList.size());
      updateRecatStatus(recatProcess, productRecatStatusList);
      recatProcessService.saveRecatProcess(recatProcess);
      log.info("Pre-process completed for Recat-request-code : {} ", recatProcess.getRecatRequestCode());
    } catch (Exception e) {
      log.error("Error when processing new recat-request-code : {} , error - ",
          recatProcess.getRecatRequestCode(), e);
      recatProcess.setStatus(RecatConstants.CANCELLED);
      recatProcess.setNotes(Constant.SYSTEM_ERROR);
      recatProcessService.saveRecatProcess(recatProcess);
    }
  }

  private void saveProductRecatData(String storeId, Collection<ProductRecatStatus> productRecatStatusList) {
    Iterables.partition(productRecatStatusList, Integer.parseInt(systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE).getValue()))
        .forEach(this::saveProductRecatStatusList);
  }

  private void updateRecatStatus(RecatProcess recatProcess, Collection<ProductRecatStatus> productRecatStatusList) {
    if (productRecatStatusList.size() == 0) {
      recatProcess.setStatus(RecatConstants.CANCELLED);
      recatProcess.setNotes(RecatConstants.FILE_IS_EMPTY);
      log.error("File is empty, Recat-request-code : {} is cancelled ", recatProcess.getRecatRequestCode());
    } else {
      recatProcess.setStartTime(new Date());
      recatProcess.setStatus(RecatConstants.IN_PROGRESS);
    }
  }

  public Collection<ProductRecatStatus> getProductRecatDataFromExcel(RecatProcess recatProcess) throws IOException {
    Sheet excelSheetData = fileStorageService.getFileDataWithInternalUploadRequest(
        BulkInternalUploadRequestDTO.builder().fileName(recatProcess.getFileName()).relativePath(RECAT)
            .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION)
            .internalProcessRequestCode(recatProcess.getRecatRequestCode()).build());
    Workbook workbook = null;
    try {
      if (excelSheetData instanceof XSSFSheet) {
        workbook = ((XSSFSheet) excelSheetData).getWorkbook();
      } else {
        throw new IllegalArgumentException("Expected XSSFSheet, but got: " + excelSheetData.getClass().getName());
      }
      List<Map<String, String>> productDataFromExcel =
          POIUtil.readStringValueFromExcelForBulkUpdate(excelSheetData, 1, 0, 0,
              BulkInternalProcessType.RECATEGORISATION.getValue());
      log.info("File data read for Recat-request-code: {}, number of rows: {}", recatProcess.getRecatRequestCode(),
          productDataFromExcel.size());
      return productDataFromExcel.stream().map(data -> getProductRecatStatus(data, recatProcess)).collect(
              Collectors.toMap(ProductRecatStatus::getProductCode, Function.identity(), (p1, p2) -> p1, LinkedHashMap::new))
          .values();
    } catch (Exception e) {
      log.error("Error processing Excel file for Recat-request-code: {}", recatProcess.getRecatRequestCode(), e);
      throw new IOException("Failed to process Excel file.", e);
    } finally {
      closeWorkbook(workbook);
    }
  }

  public void closeWorkbook(Workbook workbook) throws IOException {
    if (Objects.nonNull(workbook)) {
      workbook.close();
    }
  }

  @Override
  public RecatProductCountResponse getProductCountsByRecatRequestCode(String storeId, String recatRequestCode) {
    RecatProcess recatProcess = recatProcessService.findRecatProcessByRecatRequestCode(storeId, recatRequestCode);
    if (RecatConstants.FINISHED.equals(recatProcess.getStatus()) || RecatConstants.FAILED
        .equals(recatProcess.getStatus()) || RecatConstants.PARTIAL_SUCCESS.equals(recatProcess.getStatus())) {
      return setRecatProductCountResponseValues(recatProcess);
    } else if (RecatConstants.IN_PROGRESS.equals(recatProcess.getStatus())) {
      Map<String, Integer> productCounts = productRecatStatusService
          .getProductCountByRecatRequestCode(recatProcess.getStoreId(), recatProcess.getRecatRequestCode());
      return setRecatProductCountResponseValuesForInProgressProcess(recatProcess, productCounts);
    } else {
      RecatProductCountResponse recatProductCountResponse = new RecatProductCountResponse();
      recatProductCountResponse.setStatus(recatProcess.getStatus());
      return recatProductCountResponse;
    }
  }

  private RecatProductCountResponse setRecatProductCountResponseValues(RecatProcess recatProcess) {
    RecatProductCountResponse recatProductCountResponse = new RecatProductCountResponse();
    recatProductCountResponse.setTotalProductCount(recatProcess.getTotalCount());
    recatProductCountResponse.setSuccessCount(recatProcess.getSuccessCount());
    recatProductCountResponse.setFailedCount(recatProcess.getErrorCount());
    recatProductCountResponse.setInProgressCount(0);
    recatProductCountResponse.setStatus(recatProcess.getStatus());
    return recatProductCountResponse;
  }

  private RecatProductCountResponse setRecatProductCountResponseValuesForInProgressProcess(RecatProcess recatProcess, Map<String, Integer> productCount) {
    RecatProductCountResponse recatProductCountResponse = new RecatProductCountResponse();
    recatProductCountResponse.setTotalProductCount(recatProcess.getTotalCount());
    recatProductCountResponse.setSuccessCount(productCount.get(RecatConstants.FINISHED).longValue());
    recatProductCountResponse.setFailedCount(productCount.get(RecatConstants.FAILED).longValue());
    recatProductCountResponse.setInProgressCount(
        productCount.get(RecatConstants.PUBLISHED).longValue() + productCount.get(RecatConstants.PENDING).longValue());
    recatProductCountResponse.setStatus(recatProcess.getStatus());
    return recatProductCountResponse;
  }

  private ProductRecatStatus getProductRecatStatus(Map<String, String> row, RecatProcess recatProcess) {
    ProductRecatStatus productRecatStatus = new ProductRecatStatus();
    productRecatStatus.setProductCode(StringUtils.trimToEmpty(row.get(RecatConstants.PRODUCT_CODE)));
    productRecatStatus.setProductName(StringUtils.trimToEmpty(row.get(RecatConstants.PRODUCT_NAME)));
    productRecatStatus.setCategoryCode(StringUtils.trimToEmpty(row.get(RecatConstants.MASTER_CATEGORY_CODE)));
    productRecatStatus.setCategoryName(StringUtils.trimToEmpty(row.get(RecatConstants.MASTER_CATEGORY_NAME)));
    productRecatStatus.setNewCategoryCode(StringUtils.trimToEmpty(row.get(RecatConstants.NEW_CATEGORY_CODE)));
    productRecatStatus.setNewCategoryName(StringUtils.trimToEmpty(row.get(RecatConstants.NEW_CATEGORY_NAME)));
    productRecatStatus.setRecatRequestCode(recatProcess.getRecatRequestCode());
    productRecatStatus.setRecatProcess(recatProcess);
    productRecatStatus.setStoreId(recatProcess.getStoreId());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, recatProcess.getCreatedBy());
    productRecatStatus.setStatus(RecatConstants.PENDING);
    return productRecatStatus;
  }

  private void saveProductRecatStatusList(List<ProductRecatStatus> productRecatStatusList) {
    productRecatStatusService.saveProductRecatStatusList(productRecatStatusList);
  }

  @Async
  @Override
  public void getFailedProductsMail(String storeId,
      String recatRequestCode, String username, String requestId) throws Exception {
    RecatProcess recatProcess = recatProcessService.getRecatProcessByRecatRequestCode(storeId, recatRequestCode);
    if(recatProcess.getStatus().equals(RecatConstants.PARTIAL_SUCCESS) ||
        recatProcess.getStatus().equals(RecatConstants.FAILED)){
      BulkDownloadRequest request = bulkDownloadServiceBeanUtil
          .getRecatFailedProductsDownloadRequest(recatRequestCode, username, requestId);
      bulkProcessDownloadService.downloadExcelFile(request, BulkProcessPath.RECAT.getValue());
      recatProcessService.sendMailForRecatFailedProducts(recatProcess, username);
    }
  }

  @Async
  @Trace(dispatcher = true)
  @Override
  public void publishPendingProducts(String storeId) {
    String uuid = UUID.randomUUID().toString();
    try {
      SystemParameterConfig processNewRequest =
          systemParameterConfigService.findValueByStoreIdAndVariable(storeId, RecatConstants.PUBLISH_PENDING_PRODUCTS);
      if (Boolean.valueOf(processNewRequest.getValue())) {
        int pendingProductsBatchSize = Integer.parseInt(systemParameterConfigService
            .findValueByStoreIdAndVariable(storeId, RecatConstants.PENDING_PRODUCTS_FETCH_LIMIT).getValue());
        log.info("Starting publish of product recat process events for batchSize : {}, id : {} ",
            pendingProductsBatchSize, uuid);
        List<ProductRecatStatus> productRecatStatusList = productRecatStatusService
            .findProductRecatStatusByStoreIdAndAndStatus(storeId, RecatConstants.PENDING, pendingProductsBatchSize);
        productRecatStatusList.forEach(productRecatStatus -> productRecatStatus.setStatus(RecatConstants.PUBLISHED));
        Lists.partition(productRecatStatusList, Integer.parseInt(systemParameterConfigService
            .findValueByStoreIdAndVariable(storeId, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE).getValue()))
            .forEach(this::saveProductRecatStatusList);
        publishProductRecatProcessEvent(storeId, productRecatStatusList);
        log.info("Completed publish of product recat process events for batchSize : {}, id : {} ",
            pendingProductsBatchSize, uuid);
      }
    } catch (Exception e) {
      log.error("Exception when publishing pending products id : {}, error ", uuid, e);
    }
  }

  private void publishProductRecatProcessEvent(String storeId, List<ProductRecatStatus> productRecatStatusList) {
    List<ProductCodeAndCategoryCodeEvent> productCodeAndCategoryCodeEventList = productRecatStatusList.stream().map(
        productRecatStatus -> ProductCodeAndCategoryCodeEvent.builder().productCode(productRecatStatus.getProductCode())
            .categoryCode(productRecatStatus.getNewCategoryCode())
            .recatRequestCode(productRecatStatus.getRecatRequestCode()).id(productRecatStatus.getId()).build())
        .collect(Collectors.toList());
    List<List<ProductCodeAndCategoryCodeEvent>> batchedProductCodeAndCategoryEvent = Lists
        .partition(productCodeAndCategoryCodeEventList, Integer.parseInt(systemParameterConfigService
            .findValueByStoreIdAndVariable(storeId, RecatConstants.PENDING_PRODUCTS_PUBLISH_BATCH_SIZE).getValue()));
    for (List<ProductCodeAndCategoryCodeEvent> productCodeAndCategoryCodeEvents : batchedProductCodeAndCategoryEvent) {
      printLogsForEachPublishedProduct(productCodeAndCategoryCodeEvents);
      kafkaProducer.send(kafkaTopicProperties.getProductRecatProcess(),
          new ProductCodeAndCategoryCodeListEvent(productCodeAndCategoryCodeEvents));
    }
  }
  @Override
  public void uploadNewRecatRequest(String storeId, String recatRequestCode, String fileName,
      String scheduledTime) {
    Boolean fileExists = fileStorageService.isFileExists(
      BulkInternalUploadRequestDTO.builder().internalProcessRequestCode(recatRequestCode)
        .fileName(fileName).bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION)
        .build());
    if (Boolean.FALSE.equals(fileExists)) {
      log.error("File not found for request : {}, name : {}", recatRequestCode, fileName);
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          GenericErrorMessages.RECAT_FILE_NOT_FOUND);
    }
    RecatProcess recatProcess =
        generateRecatProcess(storeId, recatRequestCode, fileName, getScheduledDate(scheduledTime));
    recatProcessService.saveRecatProcess(recatProcess);
    recatProcessService.sendMailForNewRecatProcess(recatProcess);
  }

  private void printLogsForEachPublishedProduct(
      List<ProductCodeAndCategoryCodeEvent> productCodeAndCategoryCodeEvents) {
    productCodeAndCategoryCodeEvents.forEach(productCodeAndCategoryCodeEvent -> log
        .info("publish product recat event for productCode : {} and categoryCode : {} and recat-request-code : {}",
            productCodeAndCategoryCodeEvent.getProductCode(), productCodeAndCategoryCodeEvent.getCategoryCode(),
            productCodeAndCategoryCodeEvent.getRecatRequestCode()));
  }

  @Override
  public void updateProductCategory(ProductCodeAndCategoryCodeListEvent productCodeAndCategoryCodeListEvent) {
    for (ProductCodeAndCategoryCodeEvent productCodeAndCategoryCodeEvent : productCodeAndCategoryCodeListEvent
        .getProductCodeAndCategoryCodeEventList()) {
      log.info("Starting recat for productCode {} and recat-request-code {} ",
          productCodeAndCategoryCodeEvent.getProductCode(), productCodeAndCategoryCodeEvent.getRecatRequestCode());
      ProductRecatStatus productRecatStatus = productRecatStatusService
          .findByIdAndStatus(productCodeAndCategoryCodeEvent.getId(), RecatConstants.PUBLISHED);
      if (Objects.nonNull(productRecatStatus)) {
        try {
          String errorMessage = validateProductRecat(productRecatStatus);
          if (StringUtils.isBlank(errorMessage)) {
            errorMessage = productRecatStatusService.updateProductCategory(productRecatStatus);
          }
          productRecatStatusService.validateResponseAndSave(productRecatStatus, errorMessage);
        } catch (Exception e) {
          log.error("Exception when trying to update product category");
          productRecatStatusService.validateResponseAndSave(productRecatStatus, Constant.SYSTEM_ERROR);
        }
      } else {
        log.error("Product not found in Published state : {} ", productCodeAndCategoryCodeEvent.getProductCode());
      }
    }
  }

  private String validateProductRecat(ProductRecatStatus productRecatStatus) {
    if (StringUtils.isBlank(productRecatStatus.getProductCode())) {
      return RecatConstants.PRODUCT_CODE_EMPTY;
    }
    if (StringUtils.isBlank(productRecatStatus.getProductName())) {
      return RecatConstants.PRODUCT_NAME_EMPTY;
    }
    if (StringUtils.isBlank(productRecatStatus.getCategoryCode())) {
      return RecatConstants.CATEGORY_CODE_EMPTY;
    }
    if (StringUtils.isBlank(productRecatStatus.getCategoryName())) {
      return RecatConstants.CATEGORY_NAME_EMPTY;
    }
    if (StringUtils.isBlank(productRecatStatus.getNewCategoryCode())) {
      return RecatConstants.NEW_CATEGORY_CODE_EMPTY;
    }
    if (StringUtils.isBlank(productRecatStatus.getNewCategoryName())) {
      return RecatConstants.NEW_CATEGORY_NAME_EMPTY;
    }
    return StringUtils.EMPTY;
  }

  private Date getScheduledDate(String scheduledTime) {
    Date recatScheduledTime = new Date();
    if (StringUtils.isNotEmpty(scheduledTime)) {
      DateFormat format = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
      try {
        recatScheduledTime = format.parse(scheduledTime);
      } catch (Exception e) {
        log.error("Error parsing input schedule time string : {}, error - ", scheduledTime, e);
        return recatScheduledTime;
      }
    }
    return recatScheduledTime;
  }

  private RecatProcess generateRecatProcess(String storeId, String recatRequestCode, String fileName,
      Date scheduledTime) {
    RecatProcess recatProcess = new RecatProcess();
    recatProcess.setRecatRequestCode(recatRequestCode);
    recatProcess.setFileName(fileName);
    recatProcess.setScheduledTime(scheduledTime);
    recatProcess.setStatus(RecatConstants.NEW);
    recatProcess.setStoreId(storeId);
    recatProcess.setTotalCount(0);
    recatProcess.setSuccessCount(0);
    recatProcess.setErrorCount(0);
    recatProcess.setInputErrorCount(0);
    recatProcess.setSystemErrorCount(0);
    return recatProcess;
  }

  @Override
  public Page<RecatProcessSummaryResponse> getRecatProcessSummary(String storeId,
      RecatProcessSummaryRequest recatProcessSummaryRequest, int page, int size) {
    Page<RecatProcess> recatProcessPage = this.recatProcessService
        .getRecatProcessSummary(storeId, recatProcessSummaryRequest, page, size);
    return new PageImpl<>(
        ResponseHelper.toRecatProcessSummaryResponseList(recatProcessPage.getContent()), PageRequest.of(page, size), recatProcessPage.getTotalElements());
  }

  @Async
  @Trace(dispatcher = true)
  @Override
  public void updateRecatProcessFinalStatus(String storeId) {
    try {
      SystemParameterConfig processNewRequest =
          systemParameterConfigService.findValueByStoreIdAndVariable(storeId, RecatConstants.CHECK_PENDING_PRODUCTS);
      if (Boolean.valueOf(processNewRequest.getValue())) {
        List<RecatProcess> inProgressRecatProcessList =
            recatProcessService.getRecatProcessByStoreIdAndStatus(storeId, RecatConstants.IN_PROGRESS);
        List<RecatProcess> sendMailForRecatProcessList = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(inProgressRecatProcessList)) {
          for (RecatProcess recatProcess : inProgressRecatProcessList) {
            log.info("Check status for recat-request-code : {} ", recatProcess.getRecatRequestCode());
            try {
              checkIfProcessIsCompleted(storeId, sendMailForRecatProcessList, recatProcess);
            } catch (Exception e) {
              log.error("Error when updating final recat status for recat-request-code {} ",
                  recatProcess.getRecatRequestCode(), e);
            }
          }
        }
        if (CollectionUtils.isNotEmpty(sendMailForRecatProcessList)) {
          recatProcessService.sendMailForFinishedRecatProcess(sendMailForRecatProcessList);
        }
      }
    } catch (Exception e) {
      log.error("Exception when trying to update final recat status, error - ", e);
    }
  }

  private void checkIfProcessIsCompleted(String storeId, List<RecatProcess> sendMailForRecatProcessList,
      RecatProcess recatProcess) {
    boolean updateFinalStatus = checkIfRecatProcessIsCompleted(storeId, recatProcess);
    if (updateFinalStatus) {
      Map<String, Integer> statusCountMap = productRecatStatusService
          .getStatusCountByStoreIdAndRecatRequestCount(storeId, recatProcess.getRecatRequestCode());
      RecatProcess updatedRecatProcess = recatProcessService.updateFinalStatus(recatProcess, statusCountMap);
      if (Objects.nonNull(updatedRecatProcess)) {
        sendMailForRecatProcessList.add(updatedRecatProcess);
      }
      recatProcessService.deleteRecatFile(
          ProcessorUtils.BULK_RECAT_DIR + recatProcess.getRecatRequestCode() + Constant.SLASH + recatProcess
              .getFileName());
      log.info("Final status updated for recat-request-code : {} ", recatProcess.getRecatRequestCode());
    }
  }

  private boolean checkIfRecatProcessIsCompleted(String storeId, RecatProcess recatProcess) {
    boolean updateFinalStatus = false;
    List<ProductRecatStatus> updateToFailedProductRecatStatus = new ArrayList<>();
    int pendingCountsProduct = productRecatStatusService
        .findCountByStoreIdAndStatusAndRecatRequestCode(storeId, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode());
    // If pending count = 0, check for for published products
    if (pendingCountsProduct == 0) {
      List<ProductRecatStatus> publishedProductRecatStatusList = productRecatStatusService
          .findByStoreIdAndStatusAndRecatRequestCode(storeId, RecatConstants.PUBLISHED,
              recatProcess.getRecatRequestCode());
      if (CollectionUtils.isEmpty(publishedProductRecatStatusList)) {
        // If there are no products in PUBLISHED state, then final status can be updated
        updateFinalStatus = true;
      } else {
        moveProductsToFailedIfEligible(storeId, updateToFailedProductRecatStatus, publishedProductRecatStatusList);
        Lists.partition(updateToFailedProductRecatStatus, Integer.parseInt(systemParameterConfigService
            .findValueByStoreIdAndVariable(storeId, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE).getValue()))
            .forEach(this::saveProductRecatStatusList);
        if (publishedProductRecatStatusList.size() == updateToFailedProductRecatStatus.size()) {
          // If all PUBLISHED state products are updated to FAILED state, then final status can be updated
          updateFinalStatus = true;
        }
      }
    } else {
      log.info("Recat-request-code {} in in progress, pending count : {} ", recatProcess.getRecatRequestCode(),
          pendingCountsProduct);
    }
    return updateFinalStatus;
  }

  private void moveProductsToFailedIfEligible(String storeId,
      List<ProductRecatStatus> updatedToFailedProductRecatStatus,
      List<ProductRecatStatus> publishedProductRecatStatusList) {
    Date thresholdTime = getThresholdTime(storeId);
    for (ProductRecatStatus productRecatStatus : publishedProductRecatStatusList) {
      if (productRecatStatus.getUpdatedDate().before(thresholdTime)) {
        productRecatStatus.setStatus(RecatConstants.FAILED);
        productRecatStatus.setSystemError(true);
        productRecatStatus.setErrorMessage(Constant.SYSTEM_ERROR);
        updatedToFailedProductRecatStatus.add(productRecatStatus);
      }
    }
  }

  private Date getThresholdTime(String storeId) {
    Integer updatedMinutesBefore = Integer.valueOf(systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD).getValue());
    Date date = new Date();
    // Convert Date to Calendar
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.MINUTE, -updatedMinutesBefore);
    return calendar.getTime();
  }

  @Override
  public void cancelRecatRequest(String storeId, String recatRequestCode, boolean forceUpdate, String userName) {
    RecatProcess recatProcess =
        recatProcessService.findRecatProcessByRecatRequestCode(storeId, recatRequestCode);
    validateRequest(recatProcess, forceUpdate);
    String currentStatus = recatProcess.getStatus();
    recatProcess.setStatus(RecatConstants.CANCELLED);
    recatProcess.setNotes(RecatConstants.CANCELLED_BY_USER + userName);
    recatProcessService.saveRecatProcess(recatProcess);
    forceUpdateInProgressRecat(storeId, currentStatus, forceUpdate, recatProcess);
  }

  private void validateRequest(RecatProcess recatProcess, boolean forceUpdate) {
    if (Objects.isNull(recatProcess)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, RecatConstants.RECAT_PROCESS_NOT_FOUND);
    }
    if (!forceUpdate && !RecatConstants.NEW.equals(recatProcess.getStatus())) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, RecatConstants.RECAT_PROCESS_INVALID_STATE);
    }
  }

  private void forceUpdateInProgressRecat(String storeId, String currentStatus, boolean forceUpdate,
      RecatProcess recatProcess) {
    if (forceUpdate && RecatConstants.IN_PROGRESS.equals(currentStatus)) {
      List<ProductRecatStatus> productRecatStatusList = productRecatStatusService
          .findByStoreIdAndStatusAndRecatRequestCode(storeId, RecatConstants.PENDING,
              recatProcess.getRecatRequestCode());
      if (CollectionUtils.isNotEmpty(productRecatStatusList)) {
        for (ProductRecatStatus productRecatStatus : productRecatStatusList) {
          productRecatStatus.setStatus(RecatConstants.FAILED);
          productRecatStatus.setSystemError(true);
          productRecatStatus.setErrorMessage(RecatConstants.CANCELLED_BY_SYSTEM);
        }
        Lists.partition(productRecatStatusList, Integer.parseInt(systemParameterConfigService
            .findValueByStoreIdAndVariable(storeId, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE).getValue()))
            .forEach(this::saveProductRecatStatusList);
      }
    }
  }
}
