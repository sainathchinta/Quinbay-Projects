package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.IPRProductsUploadEmailTemplate;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkAddReviewIPRProductsRequestData;
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
public class BulkIPRProductServiceImpl implements BulkIPRProductService {

  private static final String BULK_INTERNAL_REQUEST_CODE = "bulkInternalRequestCode";

  @Value("${ipr.product.add.review.error.file.path}")
  private String iprProductAddReviewErrorFilePath;


  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FileStorageService fileStorageService;

  @Override
  public void setFinalStatusAndGenerateFailedExcelForBulkAddReviewIPRProducts(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception {
    setFinalStatusAndGenerateFailedExcel(storeId, bulkInternalProcess,
        iprProductAddReviewErrorFilePath, EmailConstants.BULK_IPR_PRODUCT_ADD_REVIEW_TEMPLATE_MAP,
        EmailConstants.BULK_IPR_PRODUCT_ADD_REVIEW_SUBJECT_MAP);

  }

  private void setFinalStatusAndGenerateFailedExcel(String storeId,
      BulkInternalProcess bulkInternalProcess, String errorFileDirectory,
      Map<String, String> statusToEmailTemplateIdMap, Map<String, String> statusToEmailSubject)
      throws Exception {
    List<BulkInternalProcessData> internalProcessDataList =
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
            storeId, bulkInternalProcess.getId());
    if (CollectionUtils.isNotEmpty(internalProcessDataList) && isProcessCompleted(
        internalProcessDataList)) {
      Map<Integer, BulkInternalProcessData> excelRowDataMap = new HashMap<>();
      excelRowDataMap =
          groupDataByExcelRowNumberForIPRProductBulkAddReview(internalProcessDataList);
      int failureCount = (int) getFailureCount(internalProcessDataList);
      String errorFilePath =
          errorFileDirectory + bulkInternalProcess.getInternalProcessRequestCode()
              + ProcessorUtils.FILETYPE_XLSX_EXCEL;
      IPRProductsUploadEmailTemplate iprProductsUploadEmailTemplate =
          processFinalStatusForBulkIPRProductsAddReview(bulkInternalProcess, excelRowDataMap,
              failureCount, errorFilePath, statusToEmailTemplateIdMap, statusToEmailSubject);
      sendEmailNotification(iprProductsUploadEmailTemplate.getEmailTemplateId(),
          iprProductsUploadEmailTemplate.getEmailTemplate(),
          bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess.getCreatedBy(),
          String.valueOf(bulkInternalProcess.getTotalCount()),
          String.valueOf(iprProductsUploadEmailTemplate.getSuccessCount()),
          String.valueOf(iprProductsUploadEmailTemplate.getFailureCount()));
    }
  }

  private long getFailureCount(List<BulkInternalProcessData> internalProcessDataList) {
    return internalProcessDataList.stream().filter(
        bulkInternalProcessData -> ProcessStatus.FAILED.name()
            .equals(bulkInternalProcessData.getStatus())).count();
  }

  private IPRProductsUploadEmailTemplate processFinalStatusForBulkIPRProductsAddReview(
      BulkInternalProcess bulkInternalProcess,
      Map<Integer, BulkInternalProcessData> excelRowDataMap, int failureCount, String errorFilePath,
      Map<String, String> statusToEmailTemplateIdMap, Map<String, String> statusToEmailSubject)
      throws Exception {
    IPRProductsUploadEmailTemplate iprProductsUploadEmailTemplate =
        new IPRProductsUploadEmailTemplate();
    if (failureCount == 0) {
      updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null,
          failureCount);
      iprProductsUploadEmailTemplate.setEmailTemplateId(
          statusToEmailTemplateIdMap.get(ProcessStatus.COMPLETED.name()));
      iprProductsUploadEmailTemplate.setEmailTemplate(
          statusToEmailSubject.get(ProcessStatus.COMPLETED.name()));
      iprProductsUploadEmailTemplate.setSuccessCount(excelRowDataMap.size());
    } else if (failureCount == excelRowDataMap.size()) {
      bulkInternalProcess =
          updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(),
              errorFilePath, failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      iprProductsUploadEmailTemplate.setEmailTemplateId(
          statusToEmailTemplateIdMap.get(ProcessStatus.FAILED.name()));
      iprProductsUploadEmailTemplate.setEmailTemplate(
          statusToEmailSubject.get(ProcessStatus.FAILED.name()));
      iprProductsUploadEmailTemplate.setSuccessCount(excelRowDataMap.size() - failureCount);
      iprProductsUploadEmailTemplate.setFailureCount(failureCount);
      iprProductsUploadEmailTemplate.setTotalCount(excelRowDataMap.size());
    } else {
      bulkInternalProcess =
          updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(),
              errorFilePath, failureCount);
      generateErrorFile(bulkInternalProcess, excelRowDataMap);
      iprProductsUploadEmailTemplate.setEmailTemplateId(
          statusToEmailTemplateIdMap.get(ProcessStatus.PARTIAL_COMPLETED.name()));
      iprProductsUploadEmailTemplate.setEmailTemplate(
          statusToEmailSubject.get(ProcessStatus.PARTIAL_COMPLETED.name()));
      iprProductsUploadEmailTemplate.setSuccessCount(
          bulkInternalProcess.getTotalCount() - failureCount);
      iprProductsUploadEmailTemplate.setFailureCount(failureCount);
      iprProductsUploadEmailTemplate.setTotalCount(bulkInternalProcess.getTotalCount());
    }
    return iprProductsUploadEmailTemplate;
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

  private boolean isProcessCompleted(List<BulkInternalProcessData> internalProcessDataList) {
    return !internalProcessDataList.stream().anyMatch(
        bulkInternalProcessData -> !Set.of(ProcessStatus.COMPLETED.name(),
            ProcessStatus.FAILED.name()).contains(bulkInternalProcessData.getStatus()));
  }

  private Map<Integer, BulkInternalProcessData> groupDataByExcelRowNumberForIPRProductBulkAddReview(
      List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    Map<Integer, BulkInternalProcessData> excelRowDataMap = new HashMap<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      excelRowDataMap.put(objectMapper.readValue(bulkInternalProcessData.getData(),
          BulkAddReviewIPRProductsRequestData.class).getExcelRowNumber(), bulkInternalProcessData);
    }
    return excelRowDataMap;
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
      String username, String totalCount, String successCount, String failureCount)
      throws Exception {
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
