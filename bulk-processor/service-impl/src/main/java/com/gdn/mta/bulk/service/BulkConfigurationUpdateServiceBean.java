package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.models.EmailConstants.BULK_CONFIG_CATEGORY_ID;
import static com.gdn.mta.bulk.models.EmailConstants.BULK_CONFIG_CATEGORY_PARTIAL_ID;
import static com.gdn.mta.bulk.models.EmailConstants.BULK_CONFIG_MERCHANT_ID;
import static com.gdn.mta.bulk.models.EmailConstants.BULK_CONFIG_MERCHANT_PARTIAL_ID;
import static com.gdn.mta.bulk.models.EmailConstants.CONFIGURATION_CATEGORY_PARTIAL_SUBJECT;
import static com.gdn.mta.bulk.models.EmailConstants.CONFIGURATION_CATEGORY_SUBJECT;
import static com.gdn.mta.bulk.models.EmailConstants.CONFIGURATION_SELLER_PARTIAL_SUBJECT;
import static com.gdn.mta.bulk.models.EmailConstants.CONFIGURATION_SELLER_SUBJECT;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ConfigurationValues;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.entity.BulkInternalEventModel;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.MessageUtil;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.BulkConfigurationUpdateParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.google.common.collect.Lists;

@Service
public class BulkConfigurationUpdateServiceBean implements BulkConfigurationUpdateService {

  @Autowired
  private SystemParameter systemParameter;

  @Autowired
  private BulkConfigurationService bulkConfigurationService;

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Autowired
  private FileStorageService fileStorageService;

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkConfigurationUpdateServiceBean.class);
  private static final String lang = "in";
  private static final String SUCCESS_COUNT = "successFullProducts";
  private static final String TOTAL_COUNT = "totalNumberOfProducts";

  @Override
  public void processConfigurationUpdate(BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest)
      throws Exception {
    LOGGER.info(Constant.BULK_CONFIGURATION_UPDATE_LOG_MESSAGE, bulkConfigurationUpdateRequest.getBulkProcessCode(),
        bulkConfigurationUpdateRequest.getStoreId(), bulkConfigurationUpdateRequest.getUpdatedBy());
    BulkInternalProcess existingBulkProcess = internalProcessService
        .findByInternalProcessRequestCode(bulkConfigurationUpdateRequest.getStoreId(),
            bulkConfigurationUpdateRequest.getBulkProcessCode());
    if (Objects.isNull(existingBulkProcess)) {
      BulkInternalProcess bulkProcess =
          BulkUpdateServiceUtil.getBulkProcessFromBulkConfigurationUpdateRequest(bulkConfigurationUpdateRequest, 0, 0);
      BulkInternalProcess savedBulkProcess = internalProcessService.saveInternalProcess(bulkProcess);
      process(bulkConfigurationUpdateRequest, savedBulkProcess);
    }
  }

  private void process(BulkConfigurationUpdateRequest request, BulkInternalProcess bulkInternalProcess)
      throws Exception {
    Sheet excelSheetData = fileStorageService.getFileDataWithInternalUploadRequest(
      BulkInternalUploadRequestDTO.builder().relativePath(request.getFilePath())
        .bulkInternalProcessType(BulkInternalProcessType.CONFIGURATION).build());
    List<Map<String, String>> productDataFromExcel = POIUtil.readFromExcelForBulkUpdate(excelSheetData, 1, 0, 0,
        new HashMap<>());
    int totalProduct = productDataFromExcel.size();
    bulkInternalProcess.setTotalCount(totalProduct);
    bulkInternalProcess.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkInternalProcess = internalProcessService.saveInternalProcess(bulkInternalProcess);
    boolean bulkSwitch = Boolean.parseBoolean(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH).getValue());
    if (bulkSwitch) {
      this.generateBulkProcessDataForConfigUpdate(bulkInternalProcess, productDataFromExcel, request.getRequestId(),
          request.getActionType());
      return;
    }
    List<Map<String, String>> successData = new ArrayList<>();
    List<Map<String, String>> failureData = new ArrayList<>();
    validateExcelData(productDataFromExcel, successData, failureData, request.getActionType());
    processSuccessData(request, successData, failureData);
    processFailureData(bulkInternalProcess, totalProduct, successData.size(), failureData);
    updateBulkFinalStatus(bulkInternalProcess, request.getRequestId(), successData.size(), totalProduct);
  }

  private void processFailureData(BulkInternalProcess bulkInternalProcess, int totalProduct, int successData,
      List<Map<String, String>> failureData) {
    if (CollectionUtils.isNotEmpty(failureData)) {
      try {
        SXSSFWorkbook workbook = POIUtil.generateXLFileForConfiguration(failureData, bulkInternalProcess.getNotes());
        byte[] bytes = POIUtil.getByteContentFromExcel(workbook.getSheet(BulkConfigurationUpdateParameters.DATA));
        BulkUpdateProcessDTO bulkUpdateProcessDTO =
            BulkUpdateProcessDTO.builder().bulkProcessType(BulkProcessType.CONFIGURATION_ERROR.getValue())
                .fileContent(bytes).build();
        fileStorageService.createBulkFile(bulkUpdateProcessDTO, bulkInternalProcess.getInternalProcessRequestCode(),
            bulkInternalProcess.getFileName());
      } catch (Exception e) {
        LOGGER.error(Constant.FAILED_BULK_CONFIGURATION_ERROR, bulkInternalProcess.getInternalProcessRequestCode(), e);
      }
    }
    if (successData == totalProduct) {
      bulkInternalProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } else {
      if (successData != Constant.ZERO) {
        bulkInternalProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
      } else {
        bulkInternalProcess.setStatus(BulkProcess.STATUS_ABORTED);
      }
    }
  }

  private void processSuccessData(BulkConfigurationUpdateRequest request, List<Map<String, String>> successData,
      List<Map<String, String>> failureData) throws Exception {
    if (CollectionUtils.isNotEmpty(successData)) {
      if (BulkConfigurationUpdateParameters.MERCHANT.equalsIgnoreCase(request.getActionType())) {
        List<BulkMerchantConfigUploadResponse> bulkMerchantConfigUploadResponses =
            addMerchantConfigurationWithSuccessData(successData, request);
        updateMerchantFailedDataAfterUpdate(bulkMerchantConfigUploadResponses, failureData);
        removeMerchantFailedDataFromSuccessData(successData, failureData);
      } else {
        List<BulkCategoryConfigUploadResponse> bulkCategoryConfigUploadResponses =
            addCategoryConfigurationWithSuccessData(successData, request);
        updateCategoryFailedDataAfterUpdate(bulkCategoryConfigUploadResponses, failureData);
        removeCategoryFailedDataFromSuccessData(successData, failureData);
      }
    }
  }

  @Override
  public void generateBulkProcessDataForConfigUpdate(BulkInternalProcess bulkInternalProcess,
      List<Map<String, String>> productDataFromExcel, String requestId, String actionType) throws Exception {
    LOGGER.info("Inserting row data: {} {}", bulkInternalProcess.getInternalProcessRequestCode(), productDataFromExcel);
    List<BulkInternalProcessData> requestData = new ArrayList<>();
    for (Map<String, String> userData : productDataFromExcel) {
      BulkInternalProcessData bulkProcessData = new BulkInternalProcessData();
      bulkProcessData.setInternalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode());
      bulkProcessData.setStoreId(bulkInternalProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setInternalProcessRequestId(bulkInternalProcess.getId());
      bulkProcessData.setSellerCode(userData.get(BulkConfigurationUpdateParameters.REVIEW_CONFIG));
      if (BulkConfigurationUpdateParameters.MERCHANT.equalsIgnoreCase(actionType)) {
        bulkProcessData.setParentCode(userData.get(BulkConfigurationUpdateParameters.SELLER_CODE));
      } else {
        bulkProcessData.setParentCode(userData.get(BulkConfigurationUpdateParameters.CATEGORY_CODE));
      }
      bulkProcessData.setProcessType(actionType);
      bulkProcessData.setData(objectMapper.writeValueAsString(userData));
      bulkProcessData.setCreatedBy(bulkInternalProcess.getUpdatedBy());
      bulkProcessData.setUpdatedBy(bulkInternalProcess.getUpdatedBy());
      requestData.add(bulkProcessData);
    }
    LOGGER.info("Saving row data : {}", requestData);
    this.internalProcessService.saveInternalProcessData(requestData);
  }

  @Override
  public void processConfigUpdateEvent(BulkInternalEventModel bulkInternalEventModel) throws Exception {
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
    validateExcelData(userInputRows, successData, failureData, bulkInternalProcess.getNotes());
    BulkConfigurationUpdateRequest request =
        BulkConfigurationUpdateRequest.builder().requestId(bulkInternalEventModel.getRequestId())
            .actionType(bulkInternalProcess.getNotes()).filePath(bulkInternalProcess.getFileName())
            .bulkProcessCode(bulkInternalProcess.getInternalProcessRequestCode())
            .bulkProcessType(bulkInternalProcess.getProcessType()).storeId(bulkInternalEventModel.getStoreId())
            .updatedBy(bulkInternalProcess.getCreatedBy()).build();
    processSuccessData(request, successData, failureData);
    bulkUpdateServiceUtil.setBlpFinalDataForConfigUpdate(internalDataList, failureData);
    internalProcessService.saveInternalProcessData(internalDataList);
  }

  @Override
  public void setFinalStatusAndNotificationOnConfigUpdate(BulkInternalProcess bulkInternalProcess, String storeId)
      throws Exception {
    List<BulkInternalProcessData> totalData = internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId, bulkInternalProcess.getId());
    List<BulkInternalProcessData> failedInternalProcessData =
        totalData.stream().filter(data -> StringUtils.isNotEmpty(data.getErrorMessage())).collect(Collectors.toList());
    List<Map<String, String>> failureData = new ArrayList<>();
    int successSize = totalData.size() - failedInternalProcessData.size();
    for (BulkInternalProcessData failedProcessData : failedInternalProcessData) {
      LinkedHashMap<String, String> rowDataJson =
          new ObjectMapper().readValue(failedProcessData.getData(), new TypeReference<LinkedHashMap<String, String>>() {
          });
      failureData.add(rowDataJson);
    }
    processFailureData(bulkInternalProcess, totalData.size(), successSize, failureData);
    updateBulkFinalStatus(bulkInternalProcess, bulkInternalProcess.getUpdatedBy(), successSize, totalData.size());
  }

  @Override
  public void processToPublishForConfigUpdate(String storeId, String requestId,
      Page<BulkInternalProcess> bulkInternalProcesses) {
    for (BulkInternalProcess bulkProcess : bulkInternalProcesses) {
      List<BulkInternalProcessData> bulkInternalProcessData = internalProcessService
          .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(storeId,
              bulkProcess.getInternalProcessRequestCode(), BulkProcessData.STATUS_PENDING);
      bulkInternalProcessData.forEach(processData -> processData.setStatus(BulkProcessData.STATUS_IN_PROGRESS));
      List<String> productCodes =
          bulkInternalProcessData.stream().map(BulkInternalProcessData::getParentCode).collect(Collectors.toList());
      LOGGER.info("Publish event = {}, blpCode : {}", kafkaTopicProperties.getBulkConfigurationUpdateRows(),
          bulkProcess.getInternalProcessRequestCode());
      int batchSize = Integer.parseInt(systemParameterConfigService
          .findValueByStoreIdAndVariable(storeId, SystemParameterConfigNames.PROCESS_DATA_CONFIG_UPDATE_BATCH_SIZE)
          .getValue());
      List<List<String>> subRows = Lists.partition(productCodes, batchSize);
      LOGGER.info("Saving data for published entries : {}", bulkProcess.getInternalProcessRequestCode());
      internalProcessService.saveInternalProcessData(bulkInternalProcessData);
      for (List<String> batch : subRows) {
        BulkInternalEventModel bulkUpdateEventModel =
            BulkInternalEventModel.builder().storeId(storeId).requestId(requestId)
                .internalProcessRequestCode(bulkProcess.getInternalProcessRequestCode()).processCode(batch).build();
        kafkaProducer.send(kafkaTopicProperties.getBulkConfigurationUpdateRows(),
            bulkUpdateEventModel);
        LOGGER.info("event = {} got published having bulkProcessCode = {} bulkUpdateEventModel = {} ",
            kafkaTopicProperties.getBulkConfigurationUpdateRows(), bulkProcess.getInternalProcessRequestCode(),
            bulkUpdateEventModel);
      }
    }
  }

  private List<BulkMerchantConfigUploadResponse> addMerchantConfigurationWithSuccessData(
      List<Map<String, String>> successData, BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest)
      throws Exception {
    int bulkUpdateBatchSize = systemParameter.getConfigurationBulkUploadBatchSize();
    List<List<MerchantConfigurationRequest>> merchantConfigurationRequestList =
        Lists.partition(generateMerchantConfigRequestList(successData), bulkUpdateBatchSize);
    List<BulkMerchantConfigUploadResponse> errorResults = new ArrayList<>();
    for (List<MerchantConfigurationRequest> merchantConfigurationRequests : merchantConfigurationRequestList) {
      List<BulkMerchantConfigUploadResponse> errors = bulkConfigurationService
          .bulkMerchantConfigUpload(merchantConfigurationRequests, bulkConfigurationUpdateRequest);
      errorResults.addAll(errors);
    }
    LOGGER.info("Bulk merchant upload configuration failed for request : {} ", errorResults);
    return errorResults;
  }

  private List<BulkCategoryConfigUploadResponse> addCategoryConfigurationWithSuccessData(
      List<Map<String, String>> successData, BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest)
      throws Exception {
    int bulkUpdateBatchSize = systemParameter.getConfigurationBulkUploadBatchSize();
    List<List<CategoryConfigurationRequest>> categoryConfigurationList =
        Lists.partition(generateCategoryConfigRequestList(successData), bulkUpdateBatchSize);
    List<BulkCategoryConfigUploadResponse> errorResults = new ArrayList<>();
    for (List<CategoryConfigurationRequest> categoryConfigurationRequests : categoryConfigurationList) {
      List<BulkCategoryConfigUploadResponse> errors = bulkConfigurationService
          .bulkCategoryConfigUpload(categoryConfigurationRequests, bulkConfigurationUpdateRequest);
      errorResults.addAll(errors);
    }
    LOGGER.info("Bulk category upload configuration failed for request : {}", errorResults);
    return errorResults;
  }

  private List<MerchantConfigurationRequest> generateMerchantConfigRequestList(List<Map<String, String>> successData) {
    List<MerchantConfigurationRequest> merchantConfigurationRequestList = new ArrayList<>();
    for (Map<String, String> row : successData) {
      MerchantConfigurationRequest merchantConfigurationRequest = MerchantConfigurationRequest.builder()
          .businessPartnerCode(row.get(BulkConfigurationUpdateParameters.SELLER_CODE))
          .businessPartnerName(row.get(BulkConfigurationUpdateParameters.SELLER_NAME))
          .reviewConfig(row.get(BulkConfigurationUpdateParameters.REVIEW_CONFIG)).build();
      merchantConfigurationRequestList.add(merchantConfigurationRequest);
    }
    return merchantConfigurationRequestList;
  }

  private List<CategoryConfigurationRequest> generateCategoryConfigRequestList(List<Map<String, String>> successData) {
    List<CategoryConfigurationRequest> categoryConfigurationRequestList = new ArrayList<>();
    for (Map<String, String> row : successData) {
      CategoryConfigurationRequest categoryConfigurationRequest =
          CategoryConfigurationRequest.builder().categoryCode(row.get(BulkConfigurationUpdateParameters.CATEGORY_CODE))
              .categoryName(row.get(BulkConfigurationUpdateParameters.CATEGORY_NAME))
              .reviewConfig(row.get(BulkConfigurationUpdateParameters.REVIEW_CONFIG)).build();
      categoryConfigurationRequestList.add(categoryConfigurationRequest);
    }
    return categoryConfigurationRequestList;
  }

  private void updateBulkFinalStatus(BulkInternalProcess bulkProcess, String requestId, int successProductCount,
      int totalProductCount) throws Exception {
    LOGGER.info("Updating the final status of bulk configuration upload for type {} with bulk process status {} ",
        bulkProcess.getNotes(), bulkProcess.getStatus());
    bulkProcess.setEndTime(Calendar.getInstance().getTime());
    bulkProcess.setSuccessCount(successProductCount);
    bulkProcess.setErrorCount(totalProductCount - successProductCount);
    internalProcessService.saveInternalProcess(bulkProcess);
    LOGGER.info(Constant.BULK_CONFIGURATION_UPDATE_LOG_COMPLETED_MESSAGE, requestId, bulkProcess.getStoreId(),
        bulkProcess.getCreatedBy());
    String filePath =
        ProcessorUtils.BULK_CONFIGURATION_DIR + bulkProcess.getInternalProcessRequestCode() + File.separator
            + bulkProcess.getInternalProcessRequestCode() + ProcessorUtils.getFileFormat(bulkProcess.getFileName());
    BulkDownloadRequest bulkDownloadRequest =
        new BulkDownloadRequest.BulkRequestBuilder().request(bulkProcess.getInternalProcessRequestCode())
            .downloadType(DownloadType.ALL).fileType(FileType.XLSX).directDownload(false).filename(filePath)
            .emailTo(bulkProcess.getCreatedBy()).username(bulkProcess.getCreatedBy()).language(lang).build();
    LOGGER.info(
        "Sending email notification after processing the bulk configuration upload file for {} with bulkDownloadRequest {}",
        bulkProcess.getNotes(), bulkDownloadRequest);
    sendEmailNotification(bulkDownloadRequest, bulkProcess.getNotes(), successProductCount, totalProductCount);
  }

  private void sendEmailNotification(BulkDownloadRequest bulkDownloadRequest, String actionType,
      int successProductCount, int totalProductCount) throws Exception {
    Map<String, Object> emailParams;
    String filePrefix = fileStorageService.getFilePrefix(BulkProcessType.CONFIGURATION.getValue());
    if (BulkConfigurationUpdateParameters.MERCHANT.equalsIgnoreCase(actionType)
        && successProductCount == totalProductCount) {
      emailParams =
          getEmailParams(bulkDownloadRequest, CONFIGURATION_SELLER_SUBJECT, BULK_CONFIG_MERCHANT_ID, totalProductCount,
              totalProductCount);
    } else if (BulkConfigurationUpdateParameters.MERCHANT.equalsIgnoreCase(actionType) && (successProductCount
        < totalProductCount)) {
      emailParams =
          getEmailParams(bulkDownloadRequest, CONFIGURATION_SELLER_PARTIAL_SUBJECT, BULK_CONFIG_MERCHANT_PARTIAL_ID,
              totalProductCount, successProductCount);
    } else if (BulkConfigurationUpdateParameters.CATEGORY.equalsIgnoreCase(actionType) && (successProductCount
        == totalProductCount)) {
      emailParams = getEmailParams(bulkDownloadRequest, CONFIGURATION_CATEGORY_SUBJECT, BULK_CONFIG_CATEGORY_ID,
          totalProductCount, totalProductCount);
    } else if (BulkConfigurationUpdateParameters.CATEGORY.equalsIgnoreCase(actionType) && (successProductCount
        < totalProductCount)) {
      emailParams =
          getEmailParams(bulkDownloadRequest, CONFIGURATION_CATEGORY_PARTIAL_SUBJECT, BULK_CONFIG_CATEGORY_PARTIAL_ID,
              totalProductCount, successProductCount);
    } else {
      LOGGER.debug("Didn't find any data to send email for configuration upload requestId : {} username : {}",
          bulkDownloadRequest.getRequestId(), bulkDownloadRequest.getUsername());
      return;
    }
    emailParams.put(EmailConstants.FILE_PREFIX, filePrefix);
    LOGGER.info("Sending notification mail to internal user for {} configuration upload with emailDetails : {}",
        actionType, emailParams);
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

  private Sheet getExcelSheetData(String filePath) {
    try (InputStream fileInputStream = new FileInputStream(new File(filePath))) {
      return POIUtil.getSheetForInputStream(fileInputStream, filePath.endsWith(ProcessorUtils.FILETYPE_XLSX_EXCEL), 0);
    } catch (IOException e) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, e.getMessage(), e);
    }
  }

  private void validateExcelData(List<Map<String, String>> excelData, List<Map<String, String>> successData,
      List<Map<String, String>> failureData, String type) {
    for (Map<String, String> row : excelData) {
      String error = validateRowAndGetError(row, type);
      if (StringUtils.isBlank(error)) {
        successData.add(row);
      } else {
        row.put(BulkConfigurationUpdateParameters.FAILURE_REASON, error.substring(1));
        failureData.add(row);
      }
    }
    LOGGER.info("Total number of configuration failed in validation for bulk configuration update : {} for : {} ",
        failureData.size(), type);
  }

  private String validateRowAndGetError(Map<String, String> row, String type) {
    StringBuilder stringBuilder = new StringBuilder();
    if (!validateCodeData(row, type)) {
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.EMPTY_CODE_ERROR + type);
    }
    if (!validateReviewConfigData(row, type)) {
      stringBuilder.append(Constant.FAILED_REASON_SEPARATOR).append(Constant.INVALID_CONFIG_VALUE + type);
    }
    return stringBuilder.toString();
  }

  private boolean validateCodeData(Map<String, String> row, String type) {
    if (StringUtils.isBlank(row.get(BulkConfigurationUpdateParameters.MERCHANT.equalsIgnoreCase(type) ?
        BulkConfigurationUpdateParameters.SELLER_CODE :
        BulkConfigurationUpdateParameters.CATEGORY_CODE))) {
      LOGGER.error(Constant.EMPTY_CODE_ERROR, type);
      return false;
    }
    return true;
  }

  private boolean validateReviewConfigData(Map<String, String> row, String type) {
    String configValue;
    if (BulkConfigurationUpdateParameters.MERCHANT.equalsIgnoreCase(type)) {
      configValue = ConfigurationValues.getValueForMerchant(row.get(BulkConfigurationUpdateParameters.REVIEW_CONFIG));
    } else {
      configValue = ConfigurationValues.getValueForCategory(row.get(BulkConfigurationUpdateParameters.REVIEW_CONFIG));
    }
    if (Objects.isNull(configValue)) {
      LOGGER.error(Constant.INVALID_CONFIG_VALUE, type);
      return false;
    }
    row.put(BulkConfigurationUpdateParameters.REVIEW_CONFIG, configValue);
    return true;
  }

  private void removeMerchantFailedDataFromSuccessData(List<Map<String, String>> successData,
      List<Map<String, String>> failureData) {
    List<String> invalidDetails = failureData.stream().map(
        value -> value.get(BulkConfigurationUpdateParameters.SELLER_CODE)
            .concat(BulkConfigurationUpdateParameters.REVIEW_CONFIG)).collect(Collectors.toList());
    List<String> successDetails = successData.stream().map(
        value -> value.get(BulkConfigurationUpdateParameters.SELLER_CODE)
            .concat(BulkConfigurationUpdateParameters.REVIEW_CONFIG)).collect(Collectors.toList());
    successDetails.removeAll(invalidDetails);
    successData.removeIf(map -> !successDetails.contains(map.get(BulkConfigurationUpdateParameters.SELLER_CODE)
        .concat(BulkConfigurationUpdateParameters.REVIEW_CONFIG)));
    LOGGER.info(
        "Total merchant configuration successfully uploaded : {} , Total merchant configuration failed to upload : {}",
        successData, failureData);
  }

  private void updateMerchantFailedDataAfterUpdate(
      List<BulkMerchantConfigUploadResponse> bulkMerchantConfigUploadResponses, List<Map<String, String>> failureData) {
    for (BulkMerchantConfigUploadResponse response : bulkMerchantConfigUploadResponses) {
      Map<String, String> row = new HashMap<>();
      if (!StringUtils.isBlank(response.getErrorMessage()) && !Constant.SUCCESS
          .equalsIgnoreCase(response.getErrorMessage())) {
        row.put(BulkConfigurationUpdateParameters.SELLER_CODE, response.getBusinessPartnerCode());
        row.put(BulkConfigurationUpdateParameters.SELLER_NAME, response.getBusinessPartnerName());
        row.put(BulkConfigurationUpdateParameters.REVIEW_CONFIG, response.getReviewConfig());
        row.put(BulkConfigurationUpdateParameters.FAILURE_REASON, response.getErrorMessage());
        failureData.add(row);
      }
    }
  }

  private void updateCategoryFailedDataAfterUpdate(
      List<BulkCategoryConfigUploadResponse> bulkCategoryConfigUploadResponses, List<Map<String, String>> failureData) {
    for (BulkCategoryConfigUploadResponse response : bulkCategoryConfigUploadResponses) {
      Map<String, String> row = new HashMap<>();
      if (!StringUtils.isBlank(response.getErrorMessage()) && !Constant.SUCCESS
          .equalsIgnoreCase(response.getErrorMessage())) {
        row.put(BulkConfigurationUpdateParameters.CATEGORY_CODE, response.getCategoryCode());
        row.put(BulkConfigurationUpdateParameters.CATEGORY_NAME, response.getCategoryName());
        row.put(BulkConfigurationUpdateParameters.REVIEW_CONFIG, response.getReviewConfig());
        row.put(BulkConfigurationUpdateParameters.FAILURE_REASON, response.getErrorMessage());
        failureData.add(row);
      }
    }
  }

  private void removeCategoryFailedDataFromSuccessData(List<Map<String, String>> successData,
      List<Map<String, String>> failureData) {
    List<String> invalidDetails = failureData.stream().map(
        value -> value.get(BulkConfigurationUpdateParameters.CATEGORY_CODE)
            .concat(BulkConfigurationUpdateParameters.REVIEW_CONFIG)).collect(Collectors.toList());
    List<String> successDetails = successData.stream().map(
        value -> value.get(BulkConfigurationUpdateParameters.CATEGORY_CODE)
            .concat(BulkConfigurationUpdateParameters.REVIEW_CONFIG)).collect(Collectors.toList());
    successDetails.removeAll(invalidDetails);
    successData.removeIf(map -> !successDetails.contains(map.get(BulkConfigurationUpdateParameters.CATEGORY_CODE)
        .concat(BulkConfigurationUpdateParameters.REVIEW_CONFIG)));
    LOGGER.info(
        "Total category configuration successfully uploaded : {} , Total category configuration failed to upload : {}",
        successData, failureData);
  }
}
