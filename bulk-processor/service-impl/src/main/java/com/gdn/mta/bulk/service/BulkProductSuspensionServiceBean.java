package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.models.EmailConstants.BULK_PRODUCT_REACTIVATE_ID;
import static com.gdn.mta.bulk.models.EmailConstants.BULK_PRODUCT_REACTIVATE_PARTIAL_ID;
import static com.gdn.mta.bulk.models.EmailConstants.BULK_PRODUCT_SUSPENSION_ID;
import static com.gdn.mta.bulk.models.EmailConstants.BULK_PRODUCT_SUSPENSION_PARTIAL_ID;
import static com.gdn.mta.bulk.models.EmailConstants.REACTIVATE_PRODUCT_PARTIAL_SUBJECT;
import static com.gdn.mta.bulk.models.EmailConstants.REACTIVATE_PRODUCT_SUBJECT;
import static com.gdn.mta.bulk.models.EmailConstants.SUSPENSION_PRODUCT_SUBJECT;
import static com.gdn.mta.bulk.models.EmailConstants.SUSPENSION_PRODUCT__PARTIAL_SUBJECT;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.EnumUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.ProductSuspensionReasons;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.entity.BulkInternalEventModel;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.BulkProductSuspensionParameters;
import com.gdn.partners.bulk.util.Constant;
import com.google.common.collect.Lists;

@Service
@Slf4j
public class BulkProductSuspensionServiceBean implements BulkProductSuspensionService {

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private SystemParameter systemParameter;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  private static final String lang = "in";
  private static final String SUSPEND = "SUSPEND";
  private static final String REACTIVATE = "REACTIVATE";
  private static final String SUCCESS_COUNT = "successFullProducts";
  private static final String TOTAL_COUNT = "totalNumberOfProducts";
  private static final Logger LOGGER = LoggerFactory.getLogger(BulkProductSuspensionServiceBean.class);

  @Override
  public void process(BulkProductSuspensionRequest productSuspensionRequest) throws Exception {
    LOGGER.info(Constant.BULK_PRODUCT_SUSPENSION_LOG_MESSAGE, productSuspensionRequest.getStoreId(),
        productSuspensionRequest.getBulkProcessCode(), productSuspensionRequest.getUpdatedBy());
    BulkInternalProcess existingBulkProcess = internalProcessService
        .findByInternalProcessRequestCode(productSuspensionRequest.getStoreId(),
            productSuspensionRequest.getBulkProcessCode());
    if (Objects.isNull(existingBulkProcess)) {
      BulkInternalProcess bulkProcess = bulkUpdateServiceUtil.getBulkProcess(productSuspensionRequest, 0, 0);
      BulkInternalProcess savedBulkProcess = internalProcessService.saveInternalProcess(bulkProcess);
      process(productSuspensionRequest, savedBulkProcess);
    }
  }

  private void process(BulkProductSuspensionRequest suspensionRequest, BulkInternalProcess bulkInternalProcess)
      throws Exception {
    bulkInternalProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    Sheet excelSheetData = fileStorageService.getFileDataWithInternalUploadRequest(
      BulkInternalUploadRequestDTO.builder().relativePath(suspensionRequest.getFilePath()).bulkInternalProcessType(
        BulkInternalProcessType.SUSPEND).build());
    List<Map<String, String>> productDataFromExcel = POIUtil.readFromExcelForBulkUpdate(excelSheetData, 1, 0, 0,
        new HashMap<>());
    int totalProduct = productDataFromExcel.size();
    bulkInternalProcess.setTotalCount(totalProduct);
    bulkInternalProcess = internalProcessService.saveInternalProcess(bulkInternalProcess);
    boolean bulkSwitch = Boolean.parseBoolean(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH)
            .getValue());
    if (bulkSwitch) {
      this.generateBulkProcessData(bulkInternalProcess, productDataFromExcel, suspensionRequest.getRequestId(),
          suspensionRequest.getActionType());
      return;
    }
    List<Map<String, String>> successData = new ArrayList<>();
    List<Map<String, String>> failureData = new ArrayList<>();
    validateExcelData(productDataFromExcel, successData, failureData);
    List<SuspensionProductResponse> response =
        suspendProductsFromSuccessData(successData, bulkInternalProcess, suspensionRequest.getRequestId());
    updateFailedDataAfterUpdate(response, failureData);
    List<HashMap<String, String>>
      failedData = removeFailedDataFromSuccessData(successData, failureData);
    sendNotificationOnSuspension(failedData, bulkInternalProcess, successData.size(), totalProduct);
  }

  private void updateBulkFinalStatus(BulkInternalProcess bulkProcess, String actionType, int successProductCount,
      int totalProductCount) throws Exception {
    bulkProcess.setEndTime(Calendar.getInstance().getTime());
    bulkProcess.setErrorFilePath(new StringBuilder(bulkProcess.getFileName()).append(BulkUpdateServiceUtil.END_SYMBOL)
        .append(BulkUpdateServiceUtil.PRODUCTS_SUCCESSFULLY_SUSPENDED).append(successProductCount)
        .append(BulkUpdateServiceUtil.END_SYMBOL).append(BulkUpdateServiceUtil.PRODUCTS_NOT_SUSPENEDED)
        .append(totalProductCount - successProductCount).toString());
    bulkProcess.setSuccessCount(successProductCount);
    bulkProcess.setErrorCount(totalProductCount - successProductCount);
    internalProcessService.saveInternalProcess(bulkProcess);
    LOGGER.info(Constant.BULK_PRODUCT_SUSPENSION_LOG_COMPLETED_MESSAGE, bulkProcess);
    String filePath =
        ProcessorUtils.BULK_SUSPENSION_DIR + bulkProcess.getInternalProcessRequestCode() + File.separator + bulkProcess
            .getInternalProcessRequestCode() + ProcessorUtils.getFileFormat(bulkProcess.getFileName());
    BulkDownloadRequest bulkDownloadRequest =
        new BulkDownloadRequest.BulkRequestBuilder().request(bulkProcess.getInternalProcessRequestCode())
            .downloadType(DownloadType.ALL).fileType(FileType.XLSX).directDownload(false).filename(filePath)
            .emailTo(bulkProcess.getCreatedBy()).username(bulkProcess.getCreatedBy()).language(lang).build();
    sendEmailNotification(bulkDownloadRequest, actionType, successProductCount, totalProductCount);
  }

  private void sendEmailNotification(BulkDownloadRequest bulkDownloadRequest, String actionType,
      int successProductCount, int totalProductCount) throws Exception {
    String filePrefix = fileStorageService.getFilePrefix(BulkProcessType.PRODUCT_SUSPENSION.getValue());
    Map<String, Object> emailParams = new HashMap<>();
    if (successProductCount == totalProductCount && SUSPEND.equals(actionType)) {
      emailParams =
          getEmailParams(bulkDownloadRequest, SUSPENSION_PRODUCT_SUBJECT, BULK_PRODUCT_SUSPENSION_ID, totalProductCount,
              totalProductCount);
    } else if ((successProductCount < totalProductCount) && SUSPEND.equals(actionType)) {
      emailParams =
          getEmailParams(bulkDownloadRequest, SUSPENSION_PRODUCT__PARTIAL_SUBJECT, BULK_PRODUCT_SUSPENSION_PARTIAL_ID,
              totalProductCount, successProductCount);
    } else if ((successProductCount == totalProductCount) && REACTIVATE.equals(actionType)) {
      emailParams =
          getEmailParams(bulkDownloadRequest, REACTIVATE_PRODUCT_SUBJECT, BULK_PRODUCT_REACTIVATE_ID, totalProductCount,
              totalProductCount);
    } else if ((successProductCount < totalProductCount) && REACTIVATE.equals(actionType)) {
      emailParams =
          getEmailParams(bulkDownloadRequest, REACTIVATE_PRODUCT_PARTIAL_SUBJECT, BULK_PRODUCT_REACTIVATE_PARTIAL_ID,
              totalProductCount, successProductCount);
    } else {
      LOGGER.debug("Didn't find the data to send email for requestId : {}", bulkDownloadRequest.getRequestId());
      return;
    }
    emailParams.put(EmailConstants.FILE_PREFIX, filePrefix);
    this.mailDeliveryService.sendEmail(bulkDownloadRequest, emailParams);
  }

  private Map<String, Object> getEmailParams(BulkDownloadRequest request, String templateName, String templateId,
      int totalCount, int successCount) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, request.getUsername().split("@")[0]);
    emailParameters.put(EmailConstants.REQ_ID, request.getRequestId());
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, MessageUtil.getMessage(templateId, lang));
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, templateName);
    emailParameters.put(TOTAL_COUNT, totalCount);
    emailParameters.put(SUCCESS_COUNT, successCount);
    return emailParameters;
  }

  private void validateExcelData(List<Map<String, String>> excelData, List<Map<String, String>> successData,
      List<Map<String, String>> failureData) {
    for (Map<String, String> row : excelData) {
      String error = validateRowAndGetError(row);
      if (StringUtils.isBlank(error)) {
        successData.add(row);
      } else {
        row.put(BulkProductSuspensionParameters.FAILURE_REASON, error.substring(1));
        failureData.add(row);
      }
    }
    LOGGER.info("Total number of products failed in validation for product suspension : {}", failureData.size());
  }

  private List<SuspensionProductResponse> suspendProductsFromSuccessData(List<Map<String, String>> successData,
      BulkInternalProcess bulkInternalProcess, String requestId) throws Exception {
    int bulkUpdateBatchSize = systemParameter.getProductSuspensionBulkUploadBatchSize();
    List<List<SuspensionProductRequest>> suspensionProductRequestList = Lists
        .partition(generateSuspensionProductRequests(successData, bulkInternalProcess.getNotes()), bulkUpdateBatchSize);
    List<SuspensionProductResponse> errorResults = new ArrayList<>();
    for (List<SuspensionProductRequest> suspensionProductRequests : suspensionProductRequestList) {
      List<SuspensionProductResponse> errors = productLevel3Repository
          .doBulkSuspensionProductsActions(suspensionProductRequests, requestId, bulkInternalProcess.getUpdatedBy());
      errorResults.addAll(errors);
    }
    LOGGER.info("Bulk product suspension failed for requests : {}", errorResults);
    return errorResults;
  }

  private void updateFailedDataAfterUpdate(List<SuspensionProductResponse> response,
      List<Map<String, String>> failureData) {
    for (SuspensionProductResponse suspensionProductResponse : response) {
      Map<String, String> row = new HashMap<>();
      if (!StringUtils.isBlank(suspensionProductResponse.getErrorMessage())) {
        row.put(BulkProductSuspensionParameters.FAILURE_REASON, suspensionProductResponse.getErrorMessage());
        row.put(BulkProductSuspensionParameters.PRODUCT_CODE, suspensionProductResponse.getProductCode());
        row.put(BulkProductSuspensionParameters.SELLER_CODE, suspensionProductResponse.getBusinessPartnerCode());
        row.put(BulkProductSuspensionParameters.PRODUCT_NAME, suspensionProductResponse.getProductName());
        failureData.add(row);
      }
    }
  }

  private List<SuspensionProductRequest> generateSuspensionProductRequests(List<Map<String, String>> successData,
      String action) {
    List<SuspensionProductRequest> suspensionProductRequests = new ArrayList<>();
    for (Map<String, String> row : successData) {
      SuspensionProductRequest suspensionProductRequest =
          getSuspensionProductRequest(row.get(BulkProductSuspensionParameters.PRODUCT_CODE),
              row.get(BulkProductSuspensionParameters.PRODUCT_NAME),
              row.get(BulkProductSuspensionParameters.SELLER_CODE), row.get(BulkProductSuspensionParameters.REASON),
              row.get(BulkProductSuspensionParameters.SELLER_REASON_DESCRIPTION), action);

      suspensionProductRequests.add(suspensionProductRequest);
    }
    return suspensionProductRequests;
  }

  private SuspensionProductRequest getSuspensionProductRequest(String productCode, String productName,
      String merchantCode, String reason, String description, String action) {
    SuspensionProductRequest suspensionProductRequest = new SuspensionProductRequest();
    suspensionProductRequest.setAction(action);
    suspensionProductRequest.setNotes(description);
    suspensionProductRequest.setReason(reason);
    ProductLevel3Request productLevel3Request = new ProductLevel3Request();
    productLevel3Request.setProductCode(productCode);
    productLevel3Request.setBusinessPartnerCode(merchantCode);
    productLevel3Request.setProductName(productName);
    suspensionProductRequest.setProducts(Arrays.asList(productLevel3Request));
    return suspensionProductRequest;
  }

  private String validateRowAndGetError(Map<String, String> row) {
    StringBuilder stringBuilder = new StringBuilder();
    if (!validateProductCode(row)) {
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.SUSPENSION_EMPTY_PRODUCT_CODE_ERROR);
    }
    if (!validateMerchantCode(row)) {
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.SUSPENSION_EMPTY_MERCHANT_CODE_ERROR);
    }
    if (!validateReason(row)) {
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.SUSPENSION_INVALID_REASON_ERROR);
    }
    if (!validateReasonDescription(row)) {
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR)
          .append(Constant.SUSPENSION_INVALID_REASON_DESCRIPTION_ERROR);
    }
    return stringBuilder.toString();
  }

  private boolean validateProductCode(Map<String, String> row) {
    if (StringUtils.isBlank(row.get(BulkProductSuspensionParameters.PRODUCT_CODE))) {
      LOGGER.error(Constant.SUSPENSION_EMPTY_PRODUCT_CODE_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateMerchantCode(Map<String, String> row) {
    if (StringUtils.isBlank(row.get(BulkProductSuspensionParameters.SELLER_CODE))) {
      LOGGER.error(Constant.SUSPENSION_EMPTY_MERCHANT_CODE_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateReasonDescription(Map<String, String> row) {
    if (StringUtils.isBlank(row.get(BulkProductSuspensionParameters.SELLER_REASON_DESCRIPTION))) {
      LOGGER.error(Constant.SUSPENSION_INVALID_REASON_DESCRIPTION_ERROR, row);
      return false;
    }
    return true;
  }

  private boolean validateReason(Map<String, String> row) {
    if (StringUtils.isBlank(row.get(BulkProductSuspensionParameters.REASON))) {
      LOGGER.error(Constant.SUSPENSION_INVALID_REASON_ERROR, row);
      return false;
    }

    if (!EnumUtils.isValidEnum(ProductSuspensionReasons.class,
        row.get(BulkProductSuspensionParameters.REASON).toUpperCase().replace(StringUtils.SPACE, "_"))) {
      LOGGER.error(Constant.SUSPENSION_INVALID_REASON_ERROR, row);
      return false;
    }
    return true;
  }

  private Sheet getExcelSheetData(String filePath) {
    try (InputStream fileInputStream = new FileInputStream(new File(filePath))) {
      return POIUtil.getSheetForInputStream(fileInputStream, filePath.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    } catch (IOException e) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, e.getMessage(), e);
    }
  }


  private List<HashMap<String, String>> removeFailedDataFromSuccessData(
    List<Map<String, String>> successData, List<Map<String, String>> failureData) {
    ArrayList<HashMap<String, String>> failedData = new ArrayList<>();
    HashMap<String, String> failedDataMap = new HashMap<>();
    failureData.forEach(failedDataMap::putAll);
    Set<String> invalidDetails =
      failureData.stream().map(this::buildProductCodeAndSellerCodeKey).collect(Collectors.toSet());
    Set<String> successDetails =
      successData.stream().map(this::buildProductCodeAndSellerCodeKey).collect(Collectors.toSet());
    successDetails.removeAll(invalidDetails);
    successData.removeIf(data -> !successDetails.contains(buildProductCodeAndSellerCodeKey(data)));
    LOGGER.info("Total products successfully suspended : {} , total products suspension failed : {}", successData,
        failureData);
    failedData.add(failedDataMap);
    return failedData;
  }

  private String buildProductCodeAndSellerCodeKey(Map<String, String> value) {
    return StringUtils.defaultString(value.get(BulkProductSuspensionParameters.PRODUCT_CODE))
      + StringUtils.defaultString(value.get(BulkProductSuspensionParameters.SELLER_CODE));
  }

  @Override
  public void generateBulkProcessData(BulkInternalProcess bulkInternalProcess,
      List<Map<String, String>> productDataFromExcel, String requestId, String actionType) throws Exception {
    LOGGER.info("Inserting row data: {} {}", bulkInternalProcess.getInternalProcessRequestCode(), productDataFromExcel);
    List<BulkInternalProcessData> requestData = new ArrayList<>();
    for (Map<String, String> userData : productDataFromExcel) {
      BulkInternalProcessData bulkProcessData = new BulkInternalProcessData();
      bulkProcessData.setInternalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode());
      bulkProcessData.setStoreId(bulkInternalProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setInternalProcessRequestId(bulkInternalProcess.getId());
      bulkProcessData.setParentCode(userData.get(BulkProductSuspensionParameters.PRODUCT_CODE));
      bulkProcessData.setSellerCode(userData.get(BulkProductSuspensionParameters.SELLER_CODE));
      bulkProcessData.setProcessType(actionType);
      bulkProcessData.setErrorMessage(
        userData.getOrDefault(BulkProductSuspensionParameters.FAILURE_REASON, StringUtils.EMPTY));
      bulkProcessData.setData(objectMapper.writeValueAsString(userData));
      bulkProcessData.setCreatedBy(bulkInternalProcess.getUpdatedBy());
      bulkProcessData.setUpdatedBy(bulkInternalProcess.getUpdatedBy());
      requestData.add(bulkProcessData);
    }
    LOGGER.info("Saving row data : {}", requestData);
    this.internalProcessService.saveInternalProcessData(requestData);
  }

  @Override
  public void processSuspensionEvent(BulkInternalEventModel bulkInternalEventModel) throws Exception {
    List<Map<String, String>> successData = new ArrayList<>();
    List<Map<String, String>> failureData = new ArrayList<>();
    BulkInternalProcess bulkInternalProcess = internalProcessService
        .findByInternalProcessRequestCode(bulkInternalEventModel.getStoreId(),
            bulkInternalEventModel.getInternalProcessRequestCode());
    List<BulkInternalProcessData> internalDataList = internalProcessService
        .findByStoreIdAndBulkProcessCodeAndRowNumberIn(bulkInternalEventModel.getStoreId(),
            bulkInternalEventModel.getInternalProcessRequestCode(), bulkInternalEventModel.getProcessCode());
    internalDataList.stream()
        .forEach(bulkInternalProcessData -> bulkInternalProcessData.setStatus(ProcessStatus.PROCESSING.name()));
    internalDataList = internalProcessService.saveInternalProcessData(internalDataList);
    List<Map<String, String>> userInputRows = bulkUpdateServiceUtil.getRowDataToProcessSuspension(internalDataList);
    validateExcelData(userInputRows, successData, failureData);
    List<SuspensionProductResponse> response =
        suspendProductsFromSuccessData(successData, bulkInternalProcess, bulkInternalEventModel.getRequestId());
    updateFailedDataAfterUpdate(response, failureData);
    removeFailedDataFromSuccessData(successData, failureData);
    bulkUpdateServiceUtil.setBlpFinalDataForSuspension(internalDataList, failureData);
    internalProcessService.saveInternalProcessData(internalDataList);
  }

  @Override
  public void setFinalStatusAndNotificationOnSuspension(BulkInternalProcess bulkInternalProcess, String storeId)
      throws Exception {
    List<BulkInternalProcessData> internalProcessData = internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId, bulkInternalProcess.getId());
    List<BulkInternalProcessData> failedInternalProcessData =
        internalProcessData.stream().filter(data -> StringUtils.isNotEmpty(data.getErrorMessage()))
            .collect(Collectors.toList());
    log.info("failedInternalProcessData was : {}", failedInternalProcessData);
    List<HashMap<String, String>> failedData = new ArrayList<>();
    int successSize = internalProcessData.size() - failedInternalProcessData.size();
    for (BulkInternalProcessData failedProcessData : failedInternalProcessData) {
      LinkedHashMap<String, String> rowDataJson =
          new ObjectMapper().readValue(failedProcessData.getData(), new TypeReference<LinkedHashMap<String, String>>() {
          });
      rowDataJson.putIfAbsent(BulkProductSuspensionParameters.FAILURE_REASON,
        failedProcessData.getErrorMessage());
      failedData.add(rowDataJson);
    }
    sendNotificationOnSuspension(failedData, bulkInternalProcess, successSize, internalProcessData.size());
  }

  private void sendNotificationOnSuspension(List<HashMap<String, String>> failedData,
      BulkInternalProcess bulkInternalProcess, int successSize, int totalSize) throws Exception {
    if (CollectionUtils.isNotEmpty(failedData)) {
      try {
        SXSSFWorkbook workbook = POIUtil.generateXLFileForSuspension(failedData);
        byte[] bytes = POIUtil.getByteContentFromExcel(workbook.getSheet(BulkProductSuspensionParameters.PRODUCT_DATA));
        BulkUpdateProcessDTO bulkUpdateProcessDTO =
            BulkUpdateProcessDTO.builder().bulkProcessType(BulkProcessType.SUSPENSION_ERROR.getValue())
                .fileContent(bytes).build();
        fileStorageService.createBulkFile(bulkUpdateProcessDTO, bulkInternalProcess.getInternalProcessRequestCode(),
            bulkInternalProcess.getFileName());
      } catch (Exception e) {
        LOGGER.error(Constant.FAILED_PRODUCT_SUSPENSION_ERROR, bulkInternalProcess.getInternalProcessRequestCode(), e);
      }
    }
    if (successSize == totalSize)
      bulkInternalProcess.setStatus(ProcessStatus.COMPLETED.name());
    else {
      if (successSize != Constant.ZERO) {
        bulkInternalProcess.setStatus(ProcessStatus.PARTIAL_COMPLETED.name());
      } else {
        bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
      }
    }
    String actionType = StringUtils.equals(bulkInternalProcess.getNotes(), REACTIVATE) ?
        REACTIVATE :
        bulkInternalProcess.getProcessType();
    updateBulkFinalStatus(bulkInternalProcess,actionType, successSize, totalSize);
  }

  @Override
  public void processToPublishForSuspension(String storeId, String requestId,
      Page<BulkInternalProcess> bulkInternalProcesses) {
    for (BulkInternalProcess bulkProcess : bulkInternalProcesses) {
      List<BulkInternalProcessData> bulkInternalProcessData = internalProcessService
          .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(storeId,
              bulkProcess.getInternalProcessRequestCode(), BulkProcessData.STATUS_PENDING);
      bulkInternalProcessData.forEach(processData -> processData.setStatus(BulkProcessData.STATUS_IN_PROGRESS));
      LOGGER.info("Saving data for published entries : {}", bulkProcess.getInternalProcessRequestCode());
      internalProcessService.saveInternalProcessData(bulkInternalProcessData);
      List<String> productCodes =
          bulkInternalProcessData.stream().map(BulkInternalProcessData::getParentCode).collect(Collectors.toList());
      LOGGER.info("Publish event = {}, blpCode : {}", kafkaTopicProperties.getBulkProductSuspensionEvent(),
          bulkProcess.getInternalProcessRequestCode());
      int batchSize = Integer.parseInt(systemParameterConfigService
          .findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.PROCESS_DATA_SUSPENSION_BATCH_SIZE).getValue());
      List<List<String>> subRows = Lists.partition(productCodes, batchSize);
      for (List<String> batch : subRows) {
        BulkInternalEventModel bulkUpdateEventModel =
            BulkInternalEventModel.builder().storeId(storeId).requestId(requestId)
                .internalProcessRequestCode(bulkProcess.getInternalProcessRequestCode()).processCode(batch).build();
        kafkaProducer
            .send(kafkaTopicProperties.getBulkProductSuspensionEvent(), bulkUpdateEventModel);
        LOGGER.info("event = {} got published having bulkProcessCode = {} bulkUpdateEventModel = {} ",
            kafkaTopicProperties.getBulkProductSuspensionEvent(), bulkProcess.getInternalProcessRequestCode(),
            bulkUpdateEventModel);
      }
    }
  }
}