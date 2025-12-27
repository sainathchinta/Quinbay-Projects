package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkSkuLevelRebateRequestData;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
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

/**
 * @author akshay.n
 **/
@Service
public class BulkSkuLevelRebateServiceImpl implements BulkSkuLevelRebateService {

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Value("${gcs.sku.level.rebate.error.path}")
  private String gcsSkuLevelRebateErrorPath;

  @Value("${gcs.sku.level.rebate.upload.path}")
  private String gcsSkuLevelRebateUploadPath;

  @Value("${sku.level.rebate.listing.url}")
  private String skuLevelRebateListingURL;

  @Override
  public void sendEmailNotification(String templateId, String subject, String fileName, String username, int maxLimit)
      throws Exception {
    BulkDownloadRequest bulkDownloadRequest =
        new BulkDownloadRequest.BulkRequestBuilder().username(username).emailTo(username).build();
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, username);
    emailParameters.put(EmailConstants.TEMPLATE_ID_PARAM, templateId);
    emailParameters.put(EmailConstants.MAIL_SENDER_PARAM, EmailConstants.MAIL_SENDER);
    emailParameters.put(EmailConstants.MAIL_SUBJECT_PARAM, subject);
    emailParameters.put(EmailConstants.SKU_LEVEL_REBATE_LISTING_URL, skuLevelRebateListingURL);
    emailParameters.put(EmailConstants.FILE_NAME, fileName);
    emailParameters.put(EmailConstants.MAX_LIMIT, maxLimit);
    mailDeliveryService.sendEmail(bulkDownloadRequest, emailParameters);
  }

  @Override
  public void setFinalStatusAndGenerateErrorFileForBulkSkuLevelRebateUpdate(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception {
    List<BulkInternalProcessData> internalProcessDataList =
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId,
            bulkInternalProcess.getId());
    if (CollectionUtils.isNotEmpty(internalProcessDataList) && isProcessCompleted(internalProcessDataList)) {
      Map<Integer, BulkInternalProcessData> excelRowDataMap = groupDataByExcelRowNumber(internalProcessDataList);
      long failureCount = getFailureCount(internalProcessDataList);
      String errorFilePath = gcsSkuLevelRebateErrorPath + bulkInternalProcess.getInternalProcessRequestCode()
          + ProcessorUtils.FILETYPE_XLSX_EXCEL;
      String emailTemplateId, emailTemplate;
      if (failureCount == 0) {
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null, (int) failureCount);
        emailTemplateId = EmailConstants.BULK_SKU_LEVEL_REBATE_COMPLETED_TEMPLATE_ID;
        emailTemplate = EmailConstants.BULK_SKU_LEVEL_REBATE_COMPLETED_TEMPLATE;
      } else if (failureCount == excelRowDataMap.size()) {
        bulkInternalProcess = updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(),
            errorFilePath, (int) failureCount);
        generateErrorFile(bulkInternalProcess, excelRowDataMap);
        emailTemplateId = EmailConstants.BULK_SKU_LEVEL_REBATE_FAILED_TEMPLATE_ID;
        emailTemplate = EmailConstants.BULK_SKU_LEVEL_REBATE_FAILED_TEMPLATE;
      } else {
        bulkInternalProcess = updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(),
            errorFilePath, (int) failureCount);
        generateErrorFile(bulkInternalProcess, excelRowDataMap);
        emailTemplateId = EmailConstants.BULK_SKU_LEVEL_REBATE_PARTIALLY_COMPLETED_TEMPLATE_ID;
        emailTemplate = EmailConstants.BULK_SKU_LEVEL_REBATE_PARTIALLY_COMPLETED_TEMPLATE;
      }
      sendEmailNotification(emailTemplateId, emailTemplate, bulkInternalProcess.getFileName(),
          bulkInternalProcess.getCreatedBy(), 0);
    }
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
      excelRowDataMap.put(objectMapper.readValue(bulkInternalProcessData.getData(), BulkSkuLevelRebateRequestData.class)
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
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping = excelRowDataMap.entrySet()
        .stream()
        .filter(entry -> ProcessStatus.FAILED.name().equals(entry.getValue().getStatus()))
        .map(entry -> Pair.of(entry.getKey(), entry.getValue().getErrorMessage()))
        .collect(Collectors.toList());
    fileStorageService.downloadFileAndGenerateErrorFile(
        gcsSkuLevelRebateUploadPath + bulkInternalProcess.getCreatedBy() + Constant.SLASH
            + bulkInternalProcess.getFileName() + Constant.DOT + Constant.FILE_TYPE_XLSX,
        gcsSkuLevelRebateErrorPath + bulkInternalProcess.getCreatedBy() + Constant.SLASH
            + bulkInternalProcess.getInternalProcessRequestCode() + Constant.DOT + Constant.FILE_TYPE_XLSX,
        excelRowNumberAndErrorMessageMapping, BulkInternalProcessType.BULK_SKU_LEVEL_REBATE.name());
  }
}
