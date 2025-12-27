package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.BulkPriceUpdateRequestData;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkPriceUpdateServiceImpl implements BulkPriceUpdateService {

  private static final String SUCCESS_COUNT = "successCount";
  private static final String BULK_INTERNAL_REQUEST_CODE = "bulkInternalRequestCode";


  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Value("${gcs.bulk.price.update.error.file.path}")
  private String gcsBulkPriceUpdateErrorPath;

  @Value("${gcs.bulk.price.update.upload.file.path}")
  private String gcsBulkPriceUpdateUploadPath;

  @Override
  public void setFinalStatusAndGenerateFailedExcelForBulkPriceUpdate(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception {
    List<BulkInternalProcessData> internalProcessDataList =
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId,
            bulkInternalProcess.getId());
    if (CollectionUtils.isNotEmpty(internalProcessDataList) && isProcessCompleted(internalProcessDataList)) {
      Map<Integer, BulkInternalProcessData> excelRowDataMap = groupDataByExcelRowNumber(internalProcessDataList);
      long failureCount = getFailureCount(internalProcessDataList);
      String errorFilePath = gcsBulkPriceUpdateErrorPath + bulkInternalProcess.getInternalProcessRequestCode()
          + ProcessorUtils.FILETYPE_XLSX_EXCEL;
      String emailTemplateId, emailTemplate = StringUtils.EMPTY;
      int successCount;
      if (failureCount == 0) {
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null, (int) failureCount);
        emailTemplateId = EmailConstants.BULK_PRICE_UPDATE_COMPLETED_TEMPLATE_ID;
        emailTemplate = EmailConstants.BULK_PRICE_UPDATE_COMPLETED_TEMPLATE;
        successCount = excelRowDataMap.size();
      } else if (failureCount == excelRowDataMap.size()) {
        bulkInternalProcess =
            updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(), errorFilePath,
                (int) failureCount);
        generateErrorFile(bulkInternalProcess, excelRowDataMap);
        emailTemplateId = EmailConstants.BULK_PRICE_UPDATE_FAILED_TEMPLATE_ID;
        emailTemplate = EmailConstants.BULK_PRICE_UPDATE_FAILED_TEMPLATE;
        successCount = excelRowDataMap.size() - (int) failureCount;
      } else {
        bulkInternalProcess =
            updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(), errorFilePath,
                (int) failureCount);
        generateErrorFile(bulkInternalProcess, excelRowDataMap);
        emailTemplateId = EmailConstants.BULK_PRICE_UPDATE_PARTIALLY_COMPLETED_TEMPLATE_ID;
        emailTemplate = EmailConstants.BULK_PRICE_UPDATE_PARTIALLY_COMPLETED_TEMPLATE;
        successCount = bulkInternalProcess.getTotalCount() - (int) failureCount;
      }
      sendEmailNotification(emailTemplateId, emailTemplate, bulkInternalProcess.getInternalProcessRequestCode(),
          bulkInternalProcess.getCreatedBy(), String.valueOf(bulkInternalProcess.getTotalCount()),
          String.valueOf(successCount));
    }
  }

  private boolean isProcessCompleted(List<BulkInternalProcessData> internalProcessDataList) {
    return internalProcessDataList.stream().allMatch(
        bulkInternalProcessData -> Set.of(ProcessStatus.FINISHED.name(), ProcessStatus.FAILED.name())
            .contains(bulkInternalProcessData.getStatus()));
  }

  private Map<Integer, BulkInternalProcessData> groupDataByExcelRowNumber(
      List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    Map<Integer, BulkInternalProcessData> excelRowDataMap = new HashMap<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      excelRowDataMap.put(objectMapper.readValue(bulkInternalProcessData.getData(), BulkPriceUpdateRequestData.class)
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
        .map(entry -> Pair.of(entry.getKey(), entry.getValue().getErrorMessage())).collect(Collectors.toList());
    fileStorageService.downloadFileAndGenerateErrorFile(
        gcsBulkPriceUpdateUploadPath + bulkInternalProcess.getInternalProcessRequestCode() + Constant.SLASH
            + bulkInternalProcess.getFileName(),
        gcsBulkPriceUpdateErrorPath + bulkInternalProcess.getInternalProcessRequestCode() + Constant.DOT
            + Constant.FILE_TYPE_XLSX, excelRowNumberAndErrorMessageMapping,
        BulkInternalProcessType.BULK_PRICE_UPDATE.name());
  }

  @Override
  public void sendEmailNotification(String templateId, String subject, String requestCode, String username,
      String totalCount, String successCount) throws Exception {
    BulkDownloadRequest bulkDownloadRequest =
        new BulkDownloadRequest.BulkRequestBuilder().username(username).emailTo(username).build();
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, username);
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, templateId);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, subject);
    emailParameters.put(EmailConstants.TOTAL_COUNT, totalCount);
    emailParameters.put(SUCCESS_COUNT, successCount);
    emailParameters.put(BULK_INTERNAL_REQUEST_CODE, requestCode);
    mailDeliveryService.sendEmail(bulkDownloadRequest, emailParameters);
  }
}
