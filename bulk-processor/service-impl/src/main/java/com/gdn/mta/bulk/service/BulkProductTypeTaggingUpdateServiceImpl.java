package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.BulkPriceUpdateRequestData;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@Slf4j
public class BulkProductTypeTaggingUpdateServiceImpl implements BulkProductTypeTaggingUpdateService{

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Value("${gcs.bulk.product.type.tagging.error.file.path}")
  private String gcsBulkProductTypeTaggingErrorFilePath;

  @Value("${gcs.bulk.product.type.tagging.upload.file.path}")
  private String gcsBulkProductTypeTaggingUploadFilePath;

  private static final String BULK_INTERNAL_REQUEST_CODE = "bulkInternalRequestCode";


  @Override
  public void setFinalStatusAndGenerateFailedExcelForBulkProductTypeUpdate(String storeId,
    BulkInternalProcess bulkInternalProcess) throws Exception {
    List<BulkInternalProcessData> internalProcessDataList =
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        storeId, bulkInternalProcess.getId());
    if (CollectionUtils.isNotEmpty(internalProcessDataList) && isProcessCompleted(
      internalProcessDataList)) {
      String emailTemplateId, emailTemplate;
      int successCount;
      Map<Integer, BulkInternalProcessData> excelRowDataMap = groupDataByExcelRowNumber(internalProcessDataList);
      long failureCount = getFailureCount(internalProcessDataList);
      String errorFilePath = gcsBulkProductTypeTaggingErrorFilePath + bulkInternalProcess.getInternalProcessRequestCode()
        + ProcessorUtils.FILETYPE_XLSX_EXCEL;
      if (failureCount == 0) {
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null, (int) failureCount);
        emailTemplateId = EmailConstants.BULK_PRODUCT_TYPE_TAGGING_COMPLETED_TEMPLATE_ID;
        emailTemplate = EmailConstants.BULK_PRODUCT_TYPE_TAGGING_COMPLETED_TEMPLATE;
        successCount = excelRowDataMap.size();
    } else if (failureCount == excelRowDataMap.size()) {
      bulkInternalProcess =
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(), errorFilePath,
          (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      emailTemplateId = EmailConstants.BULK_PRODUCT_TYPE_TAGGING_MAX_ROWS_FAILURE_ID;
      emailTemplate = EmailConstants.BULK_PRODUCT_TYPE_TAGGING_MAX_ROWS_FAILURE_TEMPLATE;
      successCount = excelRowDataMap.size() - (int) failureCount;
    } else {
      bulkInternalProcess =
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(), errorFilePath,
          (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      emailTemplateId = EmailConstants.BULK_PRODUCT_TYPE_TAGGING_PARTIALLY_COMPLETED_TEMPLATE_ID;
      emailTemplate = EmailConstants.BULK_PRODUCT_TYPE_TAGGING_PARTIALLY_COMPLETED_TEMPLATE;
      successCount = bulkInternalProcess.getTotalCount() - (int) failureCount;
    }
      sendEmailNotificationForBulkProductTypeUpdate(emailTemplateId, emailTemplate, bulkInternalProcess.getInternalProcessRequestCode(),
      bulkInternalProcess.getCreatedBy(), String.valueOf(bulkInternalProcess.getTotalCount()),
      String.valueOf(successCount), errorFilePath, bulkInternalProcess.getFileName());
    }
  }

  @Override
  public void sendEmailNotificationForBulkProductTypeUpdate(String templateId, String subject,
    String requestCode, String username, String totalCount, String successCount,
    String errorFilePath, String fileName) throws Exception {
    BulkDownloadRequest bulkDownloadRequest =
      new BulkDownloadRequest.BulkRequestBuilder().username(username).emailTo(username).build();
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, username);
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, templateId);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, subject);
    emailParameters.put(EmailConstants.ERROR_FILE_PATH, errorFilePath);
    emailParameters.put(EmailConstants.TOTAL_COUNT, totalCount);
    emailParameters.put(EmailConstants.SUCCESS_COUNT, successCount);
    emailParameters.put(EmailConstants.FILE_NAME, fileName);
    emailParameters.put(BULK_INTERNAL_REQUEST_CODE, requestCode);
    mailDeliveryService.sendEmail(bulkDownloadRequest, emailParameters);
  }


  private boolean isProcessCompleted(List<BulkInternalProcessData> internalProcessDataList) {
    return internalProcessDataList.stream().allMatch(
      bulkInternalProcessData -> Set.of(ProcessStatus.COMPLETED.name(), ProcessStatus.FAILED.name())
        .contains(bulkInternalProcessData.getStatus()));
  }

  private Map<Integer, BulkInternalProcessData> groupDataByExcelRowNumber(
    List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    Map<Integer, BulkInternalProcessData> excelRowDataMap = new HashMap<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      excelRowDataMap.put(objectMapper.readValue(bulkInternalProcessData.getData(),
          BulkPriceProductTypeTaggingRequest.class)
        .getExcelRowNumber(), bulkInternalProcessData);
    }
    return excelRowDataMap;
  }

  private long getFailureCount(List<BulkInternalProcessData> internalProcessDataList) {
    return internalProcessDataList.stream()
      .filter(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus()))
      .count();
  }

  private BulkInternalProcess updateInterBulkProcessStatus(BulkInternalProcess bulkInternalProcess, String status,
    String errorFilePath, int errorCount) {
    bulkInternalProcess.setStatus(status);
    bulkInternalProcess.setErrorFilePath(errorFilePath);
    bulkInternalProcess.setErrorCount(errorCount);
    bulkInternalProcess.setSuccessCount(bulkInternalProcess.getTotalCount() - errorCount);
    bulkInternalProcess.setEndTime(new Date());
    return internalProcessService.saveInternalProcess(bulkInternalProcess);
  }

  private void generateErrorFile(BulkInternalProcess bulkInternalProcess,
    Map<Integer, BulkInternalProcessData> excelRowDataMap) throws Exception {
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping = new ArrayList<>();
    excelRowNumberAndErrorMessageMapping = excelRowDataMap.entrySet().stream()
      .filter(entry -> ProcessStatus.FAILED.name().equals(entry.getValue().getStatus()))
      .map(entry -> Pair.of(entry.getKey(), entry.getValue().getErrorMessage())).collect(
        Collectors.toList());
    fileStorageService.downloadFileAndGenerateErrorFile(
      gcsBulkProductTypeTaggingUploadFilePath + bulkInternalProcess.getInternalProcessRequestCode()
        + Constant.SLASH + bulkInternalProcess.getInternalProcessRequestCode() + Constant.DOT
        + Constant.FILE_TYPE_XLSX, gcsBulkProductTypeTaggingErrorFilePath + bulkInternalProcess.getInternalProcessRequestCode()
        + Constant.DOT + Constant.FILE_TYPE_XLSX, excelRowNumberAndErrorMessageMapping, BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
  }

}
