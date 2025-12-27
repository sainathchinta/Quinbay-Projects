package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.RestrictedKeywordProcessModel;
import com.gdn.mta.bulk.models.RestrictedKeywordRequestData;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.CategoryErrorResponse;
import com.google.common.collect.ImmutableSet;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class RestrictedKeywordServiceImpl implements RestrictedKeywordService {
  private static final String TOTAL_COUNT = "totalCount";
  private static final String SUCCESS_COUNT = "successCount";
  private static final String BULK_INTERNAL_REQUEST_CODE = "bulkInternalRequestCode";

  @Autowired
  private InternalProcessService internalProcessService;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${gcs.restricted.keyword.error.file.path}")
  private String gcsRestrictedKeywordErrorPath;

  @Override
  public void processRestrictedKeywordBulkOperation(String storeId,
      RestrictedKeywordProcessModel restrictedKeywordProcessModel) throws JsonProcessingException {
    List<BulkInternalProcessData> internalProcessDataList =
        internalProcessService.findByParentCodeAndProcessTypeAndInternalProcessRequestIdAndStatusAndMarkForDeleteFalse(
            restrictedKeywordProcessModel.getCategoryCode(), restrictedKeywordProcessModel.getType(),
            restrictedKeywordProcessModel.getInternalBulkRequestId(), ProcessStatus.PICKED.name());
    if (CollectionUtils.isNotEmpty(internalProcessDataList)) {
      try {
        internalProcessDataList = updateInterProcessDataErrorMessageAndStatus(internalProcessDataList, null,
            ProcessStatus.IN_PROGRESS.name());
        internalProcessDataList = validateDestinationCategoryInKeywordUpsert(storeId, internalProcessDataList);
        String errorMessage;
        if (CollectionUtils.isNotEmpty(internalProcessDataList)) {
          if (BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name().equals(restrictedKeywordProcessModel.getType())) {
            errorMessage = pcbOutboundService.updateCategoriesWithRestrictedKeywords(storeId,
                restrictedKeywordProcessModel.getCategoryCode(), ConverterUtil.toRestrictedKeywordAdditionRequest(internalProcessDataList));
          } else {
            errorMessage = pcbOutboundService.updateCategoriesWithRestrictedKeywords(storeId,
                restrictedKeywordProcessModel.getCategoryCode(), ConverterUtil.toRestrictedKeywordDeletionRequest(internalProcessDataList));
          }
          if (StringUtils.isNotBlank(errorMessage)) {
            internalProcessDataList = updateInterProcessDataErrorMessageAndStatus(internalProcessDataList, errorMessage,
                ProcessStatus.FAILED.name());
          } else {
            internalProcessDataList = updateInterProcessDataErrorMessageAndStatus(internalProcessDataList, errorMessage,
                ProcessStatus.COMPLETED.name());
          }
        }
      } catch (Exception e) {
        log.error("Error while updating restricted keyword for request : {} ", restrictedKeywordProcessModel, e);
        internalProcessDataList =
            updateInterProcessDataErrorMessageAndStatus(internalProcessDataList, Constant.SYSTEM_ERROR,
                ProcessStatus.FAILED.name());
      }
    }
  }

  @Override
  public void setFinalStatusAndGenerateFailedExcel(String storeId, BulkInternalProcess bulkInternalProcess)
      throws Exception {
    List<BulkInternalProcessData> internalProcessDataList =
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(storeId,
            bulkInternalProcess.getId());
    if (CollectionUtils.isNotEmpty(internalProcessDataList) && isProcessCompleted(internalProcessDataList)) {
      Map<Integer, List<BulkInternalProcessData>> excelRowDataMap = groupDataByExcelRowNumber(internalProcessDataList);
      long failureCount = getFailureCount(excelRowDataMap);
      if (failureCount == 0) {
        //All excel rows are successful
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.COMPLETED.name(), null, (int) failureCount);
        sendEmailNotification(EmailConstants.RESTRICTED_KEYWORD_COMPLETED_TEMPLATE_ID,
            EmailConstants.RESTRICTED_KEYWORD_COMPLETED_TEMPLATE_SUBJECT,
            bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess.getCreatedBy(),
            String.valueOf(bulkInternalProcess.getTotalCount()), String.valueOf(0));
      } else if (failureCount == excelRowDataMap.size()) {
        //All excel rows are failed
        String errorFilePath = gcsRestrictedKeywordErrorPath + bulkInternalProcess.getInternalProcessRequestCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL;
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.FAILED.name(), errorFilePath,
            (int) failureCount);
        generateErrorFile(bulkInternalProcess, excelRowDataMap);
        sendEmailNotification(EmailConstants.RESTRICTED_KEYWORD_FAILED_TEMPLATE_ID,
            EmailConstants.RESTRICTED_KEYWORD_FAILED_TEMPLATE_SUBJECT,
            bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess.getCreatedBy(),
            String.valueOf(bulkInternalProcess.getTotalCount()), String.valueOf(0));
      } else {
        //partial success
        String errorFilePath = gcsRestrictedKeywordErrorPath + bulkInternalProcess.getInternalProcessRequestCode() + ProcessorUtils.FILETYPE_XLSX_EXCEL;
        updateInterBulkProcessStatus(bulkInternalProcess, ProcessStatus.PARTIAL_COMPLETED.name(), errorFilePath,
            (int) failureCount);
        generateErrorFile(bulkInternalProcess, excelRowDataMap);
        sendEmailNotification(EmailConstants.RESTRICTED_KEYWORD_PARTIALLY_COMPLETED_TEMPLATE_ID,
            EmailConstants.RESTRICTED_KEYWORD_PARTIALLY_COMPLETED_TEMPLATE_SUBJECT,
            bulkInternalProcess.getInternalProcessRequestCode(), bulkInternalProcess.getCreatedBy(),
            String.valueOf(bulkInternalProcess.getTotalCount()),
            String.valueOf(bulkInternalProcess.getTotalCount() - (int) failureCount));
      }
    }
  }

  private boolean  isProcessCompleted(List<BulkInternalProcessData> internalProcessDataList) {
    return !internalProcessDataList.stream().anyMatch(
        bulkInternalProcessData -> !ImmutableSet.of(ProcessStatus.COMPLETED.name(), ProcessStatus.FAILED.name())
            .contains(bulkInternalProcessData.getStatus()));
  }

  private Map<Integer, List<BulkInternalProcessData>> groupDataByExcelRowNumber(
      List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    Map<Integer, List<BulkInternalProcessData>> excelRowDataMap = new HashMap<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      RestrictedKeywordRequestData restrictedKeywordRequestData =
          objectMapper.readValue(bulkInternalProcessData.getData(), RestrictedKeywordRequestData.class);
      List<BulkInternalProcessData> sameExcelRowData =
          excelRowDataMap.getOrDefault(restrictedKeywordRequestData.getExcelRowNumber(), new ArrayList<>());
      sameExcelRowData.add(bulkInternalProcessData);
      excelRowDataMap.put(restrictedKeywordRequestData.getExcelRowNumber(), sameExcelRowData);
    }
    return excelRowDataMap;
  }

  private long getFailureCount(Map<Integer, List<BulkInternalProcessData>> excelRowDataMap) {
    return excelRowDataMap.values().stream().filter(internalProcessDataList -> internalProcessDataList.stream()
            .anyMatch(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus())))
        .count();
  }

  private void generateErrorFile(BulkInternalProcess bulkInternalProcess,
      Map<Integer, List<BulkInternalProcessData>> excelRowDataMap) throws Exception {
    List<Pair<Integer, String>> excelRowNumberAndErrorMessageMapping = new ArrayList<>();
    for (Map.Entry<Integer, List<BulkInternalProcessData>> entry : excelRowDataMap.entrySet()) {
      List<BulkInternalProcessData> failedData = entry.getValue().stream()
          .filter(bulkInternalProcessData -> ProcessStatus.FAILED.name().equals(bulkInternalProcessData.getStatus()))
          .collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(failedData)) {
        Set<String> errorMessage = new HashSet<>();
        for (BulkInternalProcessData bulkInternalProcessData : failedData) {
          if (StringUtils.isBlank(bulkInternalProcessData.getParentCode())) {
            errorMessage.add(bulkInternalProcessData.getErrorMessage());
          } else {
            if (bulkInternalProcessData.getErrorMessage()
                .contains(BulkProcessValidationErrorMessages.CATEGORY_NOT_FOUND_ERROR_MESSAGE)) {
              errorMessage.add(bulkInternalProcessData.getParentCode() + Constant.HYPHEN + BulkProcessValidationErrorMessages.CATEGORY_NOT_FOUND_ERROR_MESSAGE_ID);
            } else {
              errorMessage.add(bulkInternalProcessData.getParentCode() + Constant.HYPHEN + BulkProcessValidationErrorMessages.CANNOT_ADD_RESTRICTED_KEYWORD_TO_CATEGORY);
            }
          }
        }
        excelRowNumberAndErrorMessageMapping.add(
            Pair.of(entry.getKey(), StringUtils.join(errorMessage, Constant.ERROR_MESSAGE_SEPARATOR)));
      }
    }
    fileStorageService.downloadFileAndGenerateErrorFile(bulkInternalProcess.getFileName(),
        bulkInternalProcess.getErrorFilePath(), excelRowNumberAndErrorMessageMapping, null);
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

  private void updateInterBulkProcessStatus(BulkInternalProcess bulkInternalProcess, String status,
      String errorFilePath, int errorCount) {
    bulkInternalProcess.setStatus(status);
    bulkInternalProcess.setErrorFilePath(errorFilePath);
    bulkInternalProcess.setErrorCount(errorCount);
    bulkInternalProcess.setSuccessCount(bulkInternalProcess.getTotalCount() - errorCount);
    bulkInternalProcess.setEndTime(new Date());
    internalProcessService.saveInternalProcess(bulkInternalProcess);
  }

  public void publishRestrictedKeywordBulkUpload(String storeId, List<BulkInternalProcess> bulkInternalProcessList,
      int fetchBatchSize) {
    for (BulkInternalProcess bulkInternalProcess : bulkInternalProcessList) {
      Set<String> categoryCodes = internalProcessService.getDistinctCategoryCodeForRestrictedKeywordBulkUpdate(storeId,
          bulkInternalProcess.getId());
      if (CollectionUtils.isNotEmpty(categoryCodes)) {
        categoryCodes = categoryCodes.stream().limit(fetchBatchSize).collect(Collectors.toSet());
        internalProcessService.bulkInterUpdatedStatusForRestrictedKeywordBulkUpdate(ProcessStatus.PICKED.name(),
            Constant.SYSTEM, storeId, bulkInternalProcess.getId(), categoryCodes);
        for (String categoryCode : categoryCodes) {
          RestrictedKeywordProcessModel restrictedKeywordProcessModel =
              RestrictedKeywordProcessModel.builder().storeId(storeId)
                  .internalBulkRequestId(bulkInternalProcess.getId())
                  .internalBulkRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
                  .categoryCode(categoryCode).type(bulkInternalProcess.getProcessType()).build();
          log.info("Category keyword update : {}, payload :{} ", kafkaTopicProperties.getRestrictedKeywordBulkProcess(),
              restrictedKeywordProcessModel);
          kafkaProducer.send(kafkaTopicProperties.getRestrictedKeywordBulkProcess(), categoryCode,
              restrictedKeywordProcessModel);
        }
      }
    }
  }

  private List<BulkInternalProcessData> updateInterProcessDataErrorMessageAndStatus(
      List<BulkInternalProcessData> internalProcessDataList, String errorMessage, String status) {
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      bulkInternalProcessData.setStatus(status);
      bulkInternalProcessData.setErrorMessage(errorMessage);
    }
    return internalProcessService.saveInternalProcessData(internalProcessDataList);
  }

  private List<BulkInternalProcessData> validateDestinationCategoryInKeywordUpsert(String storeId,
      List<BulkInternalProcessData> bulkInternalProcessDataList) throws JsonProcessingException {
    List<BulkInternalProcessData> successBulkInternalProcess = new ArrayList<>();
    List<BulkInternalProcessData> failedBulkInternalProcess = new ArrayList<>();
    Map<String, List<BulkInternalProcessData>> uniqueDestinationCategoryCodes = new HashMap<>();
    for (BulkInternalProcessData bulkInternalProcessData : bulkInternalProcessDataList) {
      RestrictedKeywordRequestData restrictedKeywordRequestData =
          objectMapper.readValue(bulkInternalProcessData.getData(), RestrictedKeywordRequestData.class);
      if (StringUtils.isBlank(restrictedKeywordRequestData.getDestinationCategory())) {
        successBulkInternalProcess.add(bulkInternalProcessData);
      } else {
        List<BulkInternalProcessData> bulkInternalProcessDataForCategory =
            uniqueDestinationCategoryCodes.getOrDefault(restrictedKeywordRequestData.getDestinationCategory(),
                new ArrayList<>());
        bulkInternalProcessDataForCategory.add(bulkInternalProcessData);
        uniqueDestinationCategoryCodes.put(restrictedKeywordRequestData.getDestinationCategory(),
            bulkInternalProcessDataForCategory);
      }
    }

    if (MapUtils.isNotEmpty(uniqueDestinationCategoryCodes)) {
      List<CategoryErrorResponse> categoryErrorResponseList = pcbOutboundService.validateDestinationCategory(storeId,
          new ArrayList<>(uniqueDestinationCategoryCodes.keySet()));
      Map<String, String> categoryErrorResponseMap = categoryErrorResponseList.stream()
          .collect(Collectors.toMap(CategoryErrorResponse::getCategoryCode, CategoryErrorResponse::getErrorMessage));
      for (Map.Entry<String, List<BulkInternalProcessData>> entry : uniqueDestinationCategoryCodes.entrySet()) {
        if (StringUtils.isNotBlank(categoryErrorResponseMap.get(entry.getKey()))) {
          for (BulkInternalProcessData bulkInternalProcessData : entry.getValue()) {
            bulkInternalProcessData.setParentCode(null);
            bulkInternalProcessData.setErrorMessage(categoryErrorResponseMap.get(entry.getKey()));
            bulkInternalProcessData.setStatus(ProcessStatus.FAILED.name());
            failedBulkInternalProcess.add(bulkInternalProcessData);
          }
        }
        else {
          successBulkInternalProcess.addAll(entry.getValue());
        }
      }
    }

    if (CollectionUtils.isNotEmpty(failedBulkInternalProcess)) {
      internalProcessService.saveInternalProcessData(failedBulkInternalProcess);
    }
    return successBulkInternalProcess;
  }
}
