package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Calendar;
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
import com.gdn.mta.bulk.models.BrandAuthAddRequestData;
import com.gdn.mta.bulk.models.BrandAuthDeleteRequestData;
import com.gdn.mta.bulk.models.BrandAuthorizationEmailTemplate;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BrandAuthorisationServiceImpl implements BrandAuthorisationService {

  private static final String SUCCESS_COUNT = "successCount";
  private static final String BULK_INTERNAL_REQUEST_CODE = "bulkInternalRequestCode";

  @Value("${gcs.brand.auth.error.file.path}")
  private String gcsBrandAuthErrorPath;
  @Autowired
  private MailDeliveryService mailDeliveryService;
  @Autowired
  private InternalProcessService internalProcessService;
  @Autowired
  private ObjectMapper objectMapper;
  @Autowired
  private FileStorageService fileStorageService;

  @Override
  public void setFinalStatusAndGenerateFailedExcel(String storeId, BulkInternalProcess bulkInternalProcess) {
    List<BulkInternalProcessData> internalProcessData = internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId, bulkInternalProcess.getId());
    List<BulkInternalProcessData> failedInternalProcessData =
        internalProcessData.stream().filter(data -> StringUtils.isNotEmpty(data.getErrorMessage()))
            .collect(Collectors.toList());
    int successSize = internalProcessData.size() - failedInternalProcessData.size();
    updateInternalBulkUploadStatus(bulkInternalProcess, successSize, bulkInternalProcess.getTotalCount());

    // TODO need to generate failed excel and send the mail

  }

  @Override
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception {
    List<BulkInternalProcessData> internalProcessDataList =
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId,
            bulkInternalProcess.getId());
    if (CollectionUtils.isNotEmpty(internalProcessDataList) && isProcessCompleted(internalProcessDataList)) {
      Map<Integer, BulkInternalProcessData> excelRowDataMap = groupDataByExcelRowNumber(internalProcessDataList);
      long failureCount = getFailureCount(internalProcessDataList);
      String processType = bulkInternalProcess.getProcessType();
      String errorFilePath = gcsBrandAuthErrorPath + bulkInternalProcess.getInternalProcessRequestCode()
          + ProcessorUtils.FILETYPE_XLSX_EXCEL;
      BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate = new BrandAuthorizationEmailTemplate();
      if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(processType)) {
        brandAuthorizationEmailTemplate =
            processFinalStatusForBrandAuthAdd(bulkInternalProcess, excelRowDataMap, failureCount, errorFilePath,
                brandAuthorizationEmailTemplate);
      } else {
        brandAuthorizationEmailTemplate =
            processFinalStatusForBrandAuthDelete(bulkInternalProcess, excelRowDataMap, failureCount, errorFilePath,
                brandAuthorizationEmailTemplate);
      }
      sendEmailNotification(brandAuthorizationEmailTemplate.getEmailTemplateId(),
          brandAuthorizationEmailTemplate.getEmailTemplate(), bulkInternalProcess.getInternalProcessRequestCode(),
          bulkInternalProcess.getCreatedBy(), String.valueOf(bulkInternalProcess.getTotalCount()),
          String.valueOf(brandAuthorizationEmailTemplate.getSuccessCount()));
    }
  }

  public BrandAuthorizationEmailTemplate processFinalStatusForBrandAuthDelete(BulkInternalProcess bulkInternalProcess,
      Map<Integer, BulkInternalProcessData> excelRowDataMap, long failureCount, String errorFilePath,
      BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate) throws Exception {
    if (failureCount == 0) {
      updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null, (int) failureCount);
      brandAuthorizationEmailTemplate.setEmailTemplateId(EmailConstants.BRAND_AUTH_DELETE_COMPLETED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(EmailConstants.BRAND_AUTH_DELETE_COMPLETED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(excelRowDataMap.size());
    } else if (failureCount == excelRowDataMap.size()) {
      bulkInternalProcess =
          updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(), errorFilePath,
              (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      brandAuthorizationEmailTemplate.setEmailTemplateId(EmailConstants.BRAND_AUTH_DELETE_FAILED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(EmailConstants.BRAND_AUTH_DELETE_FAILED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(excelRowDataMap.size() - (int) failureCount);
    } else {
      bulkInternalProcess =
          updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(), errorFilePath,
              (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      brandAuthorizationEmailTemplate.setEmailTemplateId(
          EmailConstants.BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(EmailConstants.BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(bulkInternalProcess.getTotalCount() - (int) failureCount);
    }
    return brandAuthorizationEmailTemplate;
  }

  public BrandAuthorizationEmailTemplate processFinalStatusForBrandAuthAdd(BulkInternalProcess bulkInternalProcess,
      Map<Integer, BulkInternalProcessData> excelRowDataMap, long failureCount, String errorFilePath,
      BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate) throws Exception {
    if (failureCount == 0) {
      updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null, (int) failureCount);
      brandAuthorizationEmailTemplate.setEmailTemplateId(EmailConstants.BRAND_AUTH_ADD_COMPLETED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(EmailConstants.BRAND_AUTH_ADD_COMPLETED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(excelRowDataMap.size());
    } else if (failureCount == excelRowDataMap.size()) {
      bulkInternalProcess =
          updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(), errorFilePath,
              (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      brandAuthorizationEmailTemplate.setEmailTemplateId(EmailConstants.BRAND_AUTH_ADD_FAILED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(EmailConstants.BRAND_AUTH_ADD_FAILED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(excelRowDataMap.size() - (int) failureCount);
    } else {
      bulkInternalProcess =
          updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(), errorFilePath,
              (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      brandAuthorizationEmailTemplate.setEmailTemplateId(EmailConstants.BRAND_AUTH_ADD_PARTIALLY_COMPLETED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(EmailConstants.BRAND_AUTH_ADD_PARTIALLY_COMPLETED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(bulkInternalProcess.getTotalCount() - (int) failureCount);
    }
    return brandAuthorizationEmailTemplate;
  }

  private Map<Integer, BulkInternalProcessData> groupDataByExcelRowNumber(
      List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    Map<Integer, BulkInternalProcessData> excelRowDataMap = new HashMap<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      if (BulkInternalProcessType.BRAND_AUTH_ADD.name().equals(bulkInternalProcessData.getProcessType())) {
        excelRowDataMap.put(objectMapper.readValue(bulkInternalProcessData.getData(), BrandAuthAddRequestData.class)
            .getExcelRowNumber(), bulkInternalProcessData);
      } else {
        excelRowDataMap.put(objectMapper.readValue(bulkInternalProcessData.getData(), BrandAuthDeleteRequestData.class)
            .getExcelRowNumber(), bulkInternalProcessData);
      }
    }
    return excelRowDataMap;
  }

  private void sendEmailNotification(String templateId, String subject, String requestCode, String username,
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

  private long getFailureCount(List<BulkInternalProcessData> internalProcessDataList) {
    return internalProcessDataList.stream()
        .filter(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus()))
        .count();
  }

  private boolean isProcessCompleted(List<BulkInternalProcessData> internalProcessDataList) {
    return !internalProcessDataList.stream().anyMatch(
        bulkInternalProcessData -> !Set.of(ProcessStatus.COMPLETED.name(), ProcessStatus.FAILED.name())
            .contains(bulkInternalProcessData.getStatus()));
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
    fileStorageService.downloadFileAndGenerateErrorFile(bulkInternalProcess.getFileName(),
        bulkInternalProcess.getErrorFilePath(), excelRowNumberAndErrorMessageMapping, null);
  }
  private void updateInternalBulkUploadStatus(BulkInternalProcess bulkInternalProcess, int successSize, int totalSize) {
    if (successSize == totalSize)
      bulkInternalProcess.setStatus(ProcessStatus.COMPLETED.name());
    else if (successSize != Constant.ZERO) {
      bulkInternalProcess.setStatus(ProcessStatus.PARTIAL_COMPLETED.name());
    } else {
      bulkInternalProcess.setStatus(ProcessStatus.FAILED.name());
    }
    bulkInternalProcess.setEndTime(Calendar.getInstance().getTime());
    bulkInternalProcess.setSuccessCount(successSize);
    bulkInternalProcess.setErrorCount(totalSize - successSize);
    internalProcessService.saveInternalProcess(bulkInternalProcess);
  }

}
