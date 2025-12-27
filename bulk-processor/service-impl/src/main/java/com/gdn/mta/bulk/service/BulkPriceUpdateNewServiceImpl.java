package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.BulkPriceUpdateRequestData;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.POIUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * @author akshay.n
 **/
@Service
public class BulkPriceUpdateNewServiceImpl implements BulkPriceUpdateNewService {

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Value("${gcs.new.price.update.error.path}")
  private String gcsNewPriceUpdateErrorPath;

  @Override
  public void sendEmailNotification(String templateId, String subject, String fileName, String username, int totalCount,
      int failureCount, int successCount, int maxLimit) throws Exception {
    BulkDownloadRequest bulkDownloadRequest =
        new BulkDownloadRequest.BulkRequestBuilder().username(username).emailTo(username).build();
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, username);
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, templateId);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, subject);
    emailParameters.put(EmailConstants.FILE_NAME, fileName);
    emailParameters.put(EmailConstants.FAILURE_COUNT, failureCount);
    emailParameters.put(EmailConstants.SUCCESS_COUNT, successCount);
    emailParameters.put(EmailConstants.TOTAL_COUNT, totalCount);
    emailParameters.put(EmailConstants.MAX_LIMIT, maxLimit);
    mailDeliveryService.sendEmail(bulkDownloadRequest, emailParameters);
  }

  @Override
  public void setFinalStatusAndGenerateErrorFileForBulkNewPriceUpdate(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception {
    List<BulkInternalProcessData> internalProcessDataList =
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId,
            bulkInternalProcess.getId());
    if (CollectionUtils.isNotEmpty(internalProcessDataList) && isProcessCompleted(internalProcessDataList)) {
      List<Pair<BulkPriceUpdateRequestData, String>> excelRowDataErrorMessagePairs = new ArrayList<>();
      Pair<Integer, Integer> totalAndFailedCountPair =
          groupDataByExcelRowNumberAndGetTotalAndFailedCount(excelRowDataErrorMessagePairs, internalProcessDataList);
      String errorFilePath = gcsNewPriceUpdateErrorPath + bulkInternalProcess.getInternalProcessRequestCode()
          + ProcessorUtils.FILETYPE_XLSX_EXCEL;
      String emailTemplateId, emailTemplate;
      if (totalAndFailedCountPair.getRight() == 0) {
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null,
            totalAndFailedCountPair.getRight());
        emailTemplateId = EmailConstants.BULK_NEW_PRICE_UPDATE_COMPLETED_TEMPLATE_ID;
        emailTemplate = EmailConstants.BULK_NEW_PRICE_UPDATE_COMPLETED_TEMPLATE;
      } else {
        String status = Objects.equals(totalAndFailedCountPair.getRight(), totalAndFailedCountPair.getLeft()) ?
            ProcessStatus.FAILED.name() : ProcessStatus.PARTIAL_COMPLETED.name();
        bulkInternalProcess =
            updateInterBulkProcessStatus(bulkInternalProcess, status, errorFilePath, totalAndFailedCountPair.getRight());
        generateErrorFile(bulkInternalProcess, excelRowDataErrorMessagePairs);
        emailTemplateId = EmailConstants.BULK_NEW_PRICE_UPDATE_FAILED_PARTIALLY_COMPLETED_TEMPLATE_ID;
        emailTemplate = EmailConstants.BULK_NEW_PRICE_UPDATE_FAILED_PARTIALLY_COMPLETED_TEMPLATE;
      }
      sendEmailNotification(emailTemplateId, emailTemplate, bulkInternalProcess.getFileName(),
          bulkInternalProcess.getCreatedBy(), bulkInternalProcess.getTotalCount(), totalAndFailedCountPair.getRight(),
          bulkInternalProcess.getTotalCount() - totalAndFailedCountPair.getRight(), 0);
    }
  }

  private boolean isProcessCompleted(List<BulkInternalProcessData> internalProcessDataList) {
    return internalProcessDataList.stream().allMatch(
        bulkInternalProcessData -> Set.of(ProcessStatus.FINISHED.name(), ProcessStatus.FAILED.name())
            .contains(bulkInternalProcessData.getStatus()));
  }

  private Pair<Integer, Integer> groupDataByExcelRowNumberAndGetTotalAndFailedCount(
      List<Pair<BulkPriceUpdateRequestData, String>> excelRowDataErrorMessagePairs,
      List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    Map<BulkPriceUpdateRequestData, String> finalExcelRowDataToErrorMessageMap = new HashMap<>();
    int totalCount = 0, failedCount = 0;
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      totalCount++;
      if (ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus())) {
        failedCount++;
        finalExcelRowDataToErrorMessageMap.put(objectMapper.readValue(bulkInternalProcessData.getData(),
          BulkPriceUpdateRequestData.class), bulkInternalProcessData.getErrorMessage());
      }
    }
    finalExcelRowDataToErrorMessageMap.keySet()
        .stream()
        .sorted(Comparator.comparing(BulkPriceUpdateRequestData::getExcelRowNumber))
        .forEach(bulkPriceUpdateRequestData -> {
          Pair<BulkPriceUpdateRequestData, String> bulkPriceUpdateRequestDataErrorMessagePair =
              Pair.of(bulkPriceUpdateRequestData, finalExcelRowDataToErrorMessageMap.get(bulkPriceUpdateRequestData));
          excelRowDataErrorMessagePairs.add(bulkPriceUpdateRequestDataErrorMessagePair);
        });
    return Pair.of(totalCount, failedCount);
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
      List<Pair<BulkPriceUpdateRequestData, String>> excelRowDataErrorMessagePairs) throws Exception {
    try (SXSSFWorkbook workbook = POIUtil.generateXLFileForBulkPriceUpdate(BulkParameters.BULK_PRICE_UPDATE_NEW_UPLOAD_HEADERS,
        excelRowDataErrorMessagePairs)) {
      byte[] bytes = POIUtil.getByteContentFromExcel(workbook.getSheet(BulkParameters.DATA_SHEET));
      fileStorageService.uploadToPricingBucket(
          gcsNewPriceUpdateErrorPath + bulkInternalProcess.getCreatedBy() + Constant.SLASH
              + bulkInternalProcess.getInternalProcessRequestCode() + Constant.DOT + Constant.FILE_TYPE_XLSX, bytes);
    }
  }
}
