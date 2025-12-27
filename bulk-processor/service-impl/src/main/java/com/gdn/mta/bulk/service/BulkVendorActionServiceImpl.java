package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.BrandAuthorizationEmailTemplate;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkApprovalRejectionRequestData;
import com.gdn.mta.bulk.util.ProcessorUtils;
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
public class BulkVendorActionServiceImpl implements BulkVendorActionService {

  private static final String SUCCESS_COUNT = "successCount";
  private static final String BULK_INTERNAL_REQUEST_CODE = "bulkInternalRequestCode";

  @Value("${vendor.action.error.file.path}")
  private String vendorActionErrorFilePath;

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Override
  public void setFinalStatusAndGenerateFailedExcel(String storeId,
    BulkInternalProcess bulkInternalProcess) throws Exception {
    List<BulkInternalProcessData> internalProcessDataList =
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        storeId, bulkInternalProcess.getId());
    if (CollectionUtils.isNotEmpty(internalProcessDataList) && isProcessCompleted(
      internalProcessDataList)) {
      Map<Integer, BulkInternalProcessData> excelRowDataMap =
        groupDataByExcelRowNumber(internalProcessDataList);
      long failureCount = getFailureCount(internalProcessDataList);
      String processType = bulkInternalProcess.getProcessType();
      String errorFilePath =
        vendorActionErrorFilePath + bulkInternalProcess.getInternalProcessRequestCode()
          + ProcessorUtils.FILETYPE_XLSX_EXCEL;
      BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate =
        new BrandAuthorizationEmailTemplate();
      if (BulkInternalProcessType.BULK_APPROVAL.name().equals(processType)) {
        brandAuthorizationEmailTemplate =
          processFinalStatusForBulkVendorApproval(bulkInternalProcess, excelRowDataMap,
            failureCount, errorFilePath, brandAuthorizationEmailTemplate);
      } else {
        brandAuthorizationEmailTemplate =
          processFinalStatusForBulkVendorRejection(bulkInternalProcess, excelRowDataMap,
            failureCount, errorFilePath, brandAuthorizationEmailTemplate);
      }
      sendEmailNotification(brandAuthorizationEmailTemplate.getEmailTemplateId(),
        brandAuthorizationEmailTemplate.getEmailTemplate(),
        bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess.getCreatedBy(),
        String.valueOf(bulkInternalProcess.getTotalCount()),
        String.valueOf(brandAuthorizationEmailTemplate.getSuccessCount()));
    }
  }

  private boolean isProcessCompleted(List<BulkInternalProcessData> internalProcessDataList) {
    return !internalProcessDataList.stream().anyMatch(
      bulkInternalProcessData -> !Set.of(ProcessStatus.COMPLETED.name(),
        ProcessStatus.FAILED.name()).contains(bulkInternalProcessData.getStatus()));
  }

  private Map<Integer, BulkInternalProcessData> groupDataByExcelRowNumber(
    List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    Map<Integer, BulkInternalProcessData> excelRowDataMap = new HashMap<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      if (BulkInternalProcessType.BULK_APPROVAL.name()
        .equals(bulkInternalProcessData.getProcessType())) {
        excelRowDataMap.put(
          objectMapper.readValue(bulkInternalProcessData.getData(), BulkApprovalRejectionRequestData.class)
            .getExcelRowNumber(), bulkInternalProcessData);
      } else {
        excelRowDataMap.put(
          objectMapper.readValue(bulkInternalProcessData.getData(), BulkApprovalRejectionRequestData.class)
            .getExcelRowNumber(), bulkInternalProcessData);
      }
    }
    return excelRowDataMap;
  }

  public BrandAuthorizationEmailTemplate processFinalStatusForBulkVendorApproval(
    BulkInternalProcess bulkInternalProcess, Map<Integer, BulkInternalProcessData> excelRowDataMap,
    long failureCount, String errorFilePath,
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate) throws Exception {
    if (failureCount == 0) {
      updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null,
        (int) failureCount);
      brandAuthorizationEmailTemplate.setEmailTemplateId(
        EmailConstants.VENDOR_APPROVAL_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(
        EmailConstants.VENDOR_APPROVAL_COMPLETED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(excelRowDataMap.size());
    } else if (failureCount == excelRowDataMap.size()) {
      bulkInternalProcess =
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(),
          errorFilePath, (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      brandAuthorizationEmailTemplate.setEmailTemplateId(
        EmailConstants.VENDOR_APPROVAL_FAILED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(
        EmailConstants.VENDOR_APPROVAL_FAILED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(excelRowDataMap.size() - (int) failureCount);
    } else {
      bulkInternalProcess =
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(),
          errorFilePath, (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      brandAuthorizationEmailTemplate.setEmailTemplateId(
        EmailConstants.VENDOR_APPROVAL_PARTIALLY_COMPLETED_TEMPLATE);
      brandAuthorizationEmailTemplate.setEmailTemplate(
        EmailConstants.VENDOR_APPROVAL_PARTIALLY_SUCCEED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(
        bulkInternalProcess.getTotalCount() - (int) failureCount);
    }
    return brandAuthorizationEmailTemplate;
  }

  private BulkInternalProcess updateInterBulkProcessStatus(BulkInternalProcess bulkInternalProcess,
    String status, String errorFilePath, int errorCount) {
    bulkInternalProcess.setStatus(status);
    bulkInternalProcess.setErrorFilePath(errorFilePath);
    bulkInternalProcess.setErrorCount(errorCount);
    bulkInternalProcess.setSuccessCount(bulkInternalProcess.getTotalCount() - errorCount);
    bulkInternalProcess.setEndTime(new Date());
    return internalProcessService.saveInternalProcess(bulkInternalProcess);
  }

  public BrandAuthorizationEmailTemplate processFinalStatusForBulkVendorRejection(
    BulkInternalProcess bulkInternalProcess, Map<Integer, BulkInternalProcessData> excelRowDataMap,
    long failureCount, String errorFilePath,
    BrandAuthorizationEmailTemplate brandAuthorizationEmailTemplate) throws Exception {
    if (failureCount == 0) {
      updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null,
        (int) failureCount);
      brandAuthorizationEmailTemplate.setEmailTemplateId(
        EmailConstants.VENDOR_REJECTION_COMPLETED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(
        EmailConstants.VENDOR_REJECTION_COMPLETED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(excelRowDataMap.size());
    } else if (failureCount == excelRowDataMap.size()) {
      bulkInternalProcess =
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(),
          errorFilePath, (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      brandAuthorizationEmailTemplate.setEmailTemplateId(
        EmailConstants.VENDOR_REJECTION_FAILED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(
        EmailConstants.VENDOR_REJECTION_FAILED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(excelRowDataMap.size() - (int) failureCount);
    } else {
      bulkInternalProcess =
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(),
          errorFilePath, (int) failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      brandAuthorizationEmailTemplate.setEmailTemplateId(
        EmailConstants.VENDOR_REJECTION_PARTIALLY_COMPLETED_TEMPLATE_ID);
      brandAuthorizationEmailTemplate.setEmailTemplate(
        EmailConstants.VENDOR_REJECTION_PARTIALLY_COMPLETED_TEMPLATE);
      brandAuthorizationEmailTemplate.setSuccessCount(
        bulkInternalProcess.getTotalCount() - (int) failureCount);
    }
    return brandAuthorizationEmailTemplate;
  }

  private void generateErrorFile(BulkInternalProcess bulkInternalProcess,
    Map<Integer, BulkInternalProcessData> excelRowDataMap) throws Exception {
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping = new ArrayList<>();
    excelRowNumberAndErrorMessageMapping = excelRowDataMap.entrySet().stream()
      .filter(entry -> ProcessStatus.FAILED.name().equals(entry.getValue().getStatus()))
      .map(entry -> Pair.of(entry.getKey(), entry.getValue().getErrorMessage()))
      .collect(Collectors.toList());
    fileStorageService.downloadFileAndGenerateErrorFile(bulkInternalProcess.getFileName(),
      bulkInternalProcess.getErrorFilePath(), excelRowNumberAndErrorMessageMapping, null);
  }

  private long getFailureCount(List<BulkInternalProcessData> internalProcessDataList) {
    return internalProcessDataList.stream().filter(
      bulkInternalProcessData -> ProcessStatus.FAILED.name()
        .equals(bulkInternalProcessData.getStatus())).count();
  }

  private void sendEmailNotification(String templateId, String subject, String requestCode,
    String username, String totalCount, String successCount) throws Exception {
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
