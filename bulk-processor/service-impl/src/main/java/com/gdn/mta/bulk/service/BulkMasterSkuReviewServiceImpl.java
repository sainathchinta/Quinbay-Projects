package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.MasterSkuReviewUploadEmailTemplate;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuReviewRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssigneeMasterSkuReviewRequestData;
import com.gdn.mta.bulk.util.ProcessorUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Slf4j
@Service
public class BulkMasterSkuReviewServiceImpl implements BulkMasterSkuReviewService {

  private static final String BULK_INTERNAL_REQUEST_CODE = "bulkInternalRequestCode";

  @Value("${master.sku.bulk.assignee.error.file.path}")
  private String masterSkuBulkAssigneeErrorFilePath;

  @Value("${master.sku.bulk.review.error.file.path}")
  private String masterSkuBulkReviewErrorFilePath;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FileStorageService fileStorageService;


  @Override
  public void setFinalStatusAndGenerateFailedExcelForBulkAssignee(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception {
    setFinalStatusAndGenerateFailedExcel(storeId, bulkInternalProcess, masterSkuBulkAssigneeErrorFilePath,
        EmailConstants.BULK_MASTER_SKU_ASSIGNEE_TEMPLATE_MAP, EmailConstants.BULK_MASTER_SKU_ASSIGNEE_SUBJECT_MAP);
  }

  @Override
  public void setFinalStatusAndGenerateFailedExcelForBulkMasterSkuReview(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception {
    setFinalStatusAndGenerateFailedExcel(storeId, bulkInternalProcess, masterSkuBulkReviewErrorFilePath,
        EmailConstants.BULK_MASTER_SKU_REVIEW_TEMPLATE_MAP, EmailConstants.BULK_MASTER_SKU_REVIEW_SUBJECT_MAP);
  }

  private void setFinalStatusAndGenerateFailedExcel(String storeId, BulkInternalProcess bulkInternalProcess,
      String errorFileDirectory, Map<String, String> statusToEmailTemplateIdMap,
      Map<String, String> statusToEmailSubject) throws Exception {
    List<BulkInternalProcessData> internalProcessDataList =
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId,
            bulkInternalProcess.getId());
    if (CollectionUtils.isNotEmpty(internalProcessDataList) && isProcessCompleted(internalProcessDataList)) {
      Map<Integer, BulkInternalProcessData> excelRowDataMap = new HashMap<>();
      if (BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name().equals(bulkInternalProcess.getProcessType())) {
        excelRowDataMap = groupDataByExcelRowNumberForMasterSkuReview(internalProcessDataList);
      } else {
        excelRowDataMap = groupDataByExcelRowNumberForMasterSkuAssignee(internalProcessDataList);
      }
      int failureCount = (int) getFailureCount(internalProcessDataList);
      String errorFilePath =
          errorFileDirectory + bulkInternalProcess.getInternalProcessRequestCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL;
      MasterSkuReviewUploadEmailTemplate masterSkuReviewUploadEmailTemplate =
          processFinalStatusForBulkMasterSkuReview(bulkInternalProcess, excelRowDataMap, failureCount, errorFilePath,
              statusToEmailTemplateIdMap, statusToEmailSubject);
      sendEmailNotification(masterSkuReviewUploadEmailTemplate.getEmailTemplateId(),
          masterSkuReviewUploadEmailTemplate.getEmailTemplate(), bulkInternalProcess.getInternalProcessRequestCode(),
          bulkInternalProcess.getCreatedBy(), String.valueOf(bulkInternalProcess.getTotalCount()),
          String.valueOf(masterSkuReviewUploadEmailTemplate.getSuccessCount()),
          String.valueOf(masterSkuReviewUploadEmailTemplate.getFailureCount()));
    }
  }

  private boolean isProcessCompleted(List<BulkInternalProcessData> internalProcessDataList) {
    return !internalProcessDataList.stream().anyMatch(
      bulkInternalProcessData -> !Set.of(ProcessStatus.COMPLETED.name(),
        ProcessStatus.FAILED.name()).contains(bulkInternalProcessData.getStatus()));
  }

  private Map<Integer, BulkInternalProcessData> groupDataByExcelRowNumberForMasterSkuAssignee(
    List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    Map<Integer, BulkInternalProcessData> excelRowDataMap = new HashMap<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      excelRowDataMap.put(objectMapper.readValue(bulkInternalProcessData.getData(),
        BulkAssigneeMasterSkuReviewRequestData.class).getRowNumber(), bulkInternalProcessData);
    }
    return excelRowDataMap;
  }

  private Map<Integer, BulkInternalProcessData> groupDataByExcelRowNumberForMasterSkuReview(
      List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    Map<Integer, BulkInternalProcessData> excelRowDataMap = new HashMap<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      excelRowDataMap.put(
          objectMapper.readValue(bulkInternalProcessData.getData(), BulkMasterSkuReviewRequestData.class)
              .getExcelRowNumber(), bulkInternalProcessData);
    }
    return excelRowDataMap;
  }


  private long getFailureCount(List<BulkInternalProcessData> internalProcessDataList) {
    return internalProcessDataList.stream().filter(
      bulkInternalProcessData -> ProcessStatus.FAILED.name()
        .equals(bulkInternalProcessData.getStatus())).count();
  }

  private MasterSkuReviewUploadEmailTemplate processFinalStatusForBulkMasterSkuReview(
      BulkInternalProcess bulkInternalProcess, Map<Integer, BulkInternalProcessData> excelRowDataMap, int failureCount,
      String errorFilePath, Map<String, String> statusToEmailTemplateIdMap, Map<String, String> statusToEmailSubject)
      throws Exception {
    MasterSkuReviewUploadEmailTemplate masterSkuReviewUploadEmailTemplate = new MasterSkuReviewUploadEmailTemplate();
    if (failureCount == 0) {
      updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null, failureCount);
      masterSkuReviewUploadEmailTemplate.setEmailTemplateId(
          statusToEmailTemplateIdMap.get(ProcessStatus.COMPLETED.name()));
      masterSkuReviewUploadEmailTemplate.setEmailTemplate(statusToEmailSubject.get(ProcessStatus.COMPLETED.name()));
      masterSkuReviewUploadEmailTemplate.setSuccessCount(excelRowDataMap.size());
    } else if (failureCount == excelRowDataMap.size()) {
      bulkInternalProcess =
          updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(), errorFilePath, failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      masterSkuReviewUploadEmailTemplate.setEmailTemplateId(
          statusToEmailTemplateIdMap.get(ProcessStatus.FAILED.name()));
      masterSkuReviewUploadEmailTemplate.setEmailTemplate(statusToEmailSubject.get(ProcessStatus.FAILED.name()));
      masterSkuReviewUploadEmailTemplate.setSuccessCount(excelRowDataMap.size() - failureCount);
      masterSkuReviewUploadEmailTemplate.setFailureCount(failureCount);
      masterSkuReviewUploadEmailTemplate.setTotalCount(excelRowDataMap.size());
    } else {
      bulkInternalProcess =
          updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(), errorFilePath,
              failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      masterSkuReviewUploadEmailTemplate.setEmailTemplateId(
          statusToEmailTemplateIdMap.get(ProcessStatus.PARTIAL_COMPLETED.name()));
      masterSkuReviewUploadEmailTemplate.setEmailTemplate(
          statusToEmailSubject.get(ProcessStatus.PARTIAL_COMPLETED.name()));
      masterSkuReviewUploadEmailTemplate.setSuccessCount(bulkInternalProcess.getTotalCount() - failureCount);
      masterSkuReviewUploadEmailTemplate.setFailureCount(failureCount);
      masterSkuReviewUploadEmailTemplate.setTotalCount(bulkInternalProcess.getTotalCount());
    }
    return masterSkuReviewUploadEmailTemplate;
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

  private void generateErrorFile(BulkInternalProcess bulkInternalProcess,
    Map<Integer, BulkInternalProcessData> excelRowDataMap) throws Exception {
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping =
      excelRowDataMap.entrySet().stream()
        .filter(entry -> ProcessStatus.FAILED.name().equals(entry.getValue().getStatus()))
        .map(entry -> Pair.of(entry.getKey(), entry.getValue().getErrorMessage()))
        .collect(Collectors.toList());
    fileStorageService.downloadFileAndGenerateErrorFile(bulkInternalProcess.getFileName(),
      bulkInternalProcess.getErrorFilePath(), excelRowNumberAndErrorMessageMapping, null);
  }

  private void sendEmailNotification(String templateId, String subject, String requestCode,
    String username, String totalCount, String successCount, String failureCount) throws Exception {
    BulkDownloadRequest bulkDownloadRequest =
      new BulkDownloadRequest.BulkRequestBuilder().username(username).emailTo(username).build();
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, username);
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, templateId);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, subject);
    emailParameters.put(EmailConstants.TOTAL_COUNT, totalCount);
    emailParameters.put(EmailConstants.SUCCESS_COUNT, successCount);
    emailParameters.put(EmailConstants.FAILURE_COUNT, failureCount);
    emailParameters.put(BULK_INTERNAL_REQUEST_CODE, requestCode);
    mailDeliveryService.sendEmail(bulkDownloadRequest, emailParameters);
  }
}
