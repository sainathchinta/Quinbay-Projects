package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.dto.FailedRecatProcessResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.entity.RecatProcess;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.repository.RecatProcessRepository;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.RecatConstants;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

@Slf4j
@Service
@Transactional(readOnly = true)
public class RecatProcessServiceBean implements RecatProcessService {

  private static final String DATE_FORMAT = "dd/MM/yyyy";

  @Autowired
  private RecatProcessRepository recatProcessRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private FileStorageService fileStorageService;

  @Override
  public List<RecatProcess> getAllEligibleNewRecatProcess(String storeId, String status) throws ParseException {
    List<RecatProcess> recatProcessList = recatProcessRepository.findByStoreIdAndStatus(storeId, status);
    List<RecatProcess> finalRecatProcessList = new ArrayList<>();
    Date date = new Date();
    SimpleDateFormat formatter = new SimpleDateFormat(DATE_FORMAT);
    Date currentDate = formatter.parse(formatter.format(date));
    if (CollectionUtils.isNotEmpty(recatProcessList)) {
      for (RecatProcess recatProcess : recatProcessList) {
        Date scheduledDate = formatter.parse(formatter.format(recatProcess.getScheduledTime()));
        if (scheduledDate.equals(currentDate) || scheduledDate.before(currentDate)) {
          finalRecatProcessList.add(recatProcess);
        }
      }
    }
    return finalRecatProcessList;
  }

  @Override
  @Transactional(readOnly = false)
  public void saveRecatProcess(RecatProcess recatProcess) {
    recatProcessRepository.saveAndFlush(recatProcess);
  }

  @Override
  @Transactional(readOnly = true)
  public RecatProcess getRecatProcessByRecatRequestCode(String storeId, String recatRequestCode) {
    return recatProcessRepository.findByStoreIdAndRecatRequestCode(storeId, recatRequestCode);
  }

  @Override
  public Page<RecatProcess> getRecatProcessSummary(String storeId,
      RecatProcessSummaryRequest recatProcessSummaryRequest, int page, int size) {
    return recatProcessRepository
        .findSummaryByFilter(storeId, recatProcessSummaryRequest, page, size);
  }

  @Override
  public List<RecatProcess> getRecatProcessByStoreIdAndStatus(String storeId, String status) {
    return recatProcessRepository.findByStoreIdAndStatus(storeId, status);
  }

  @Override
  @Transactional(readOnly = false)
  public RecatProcess updateFinalStatus(RecatProcess recatProcess, Map<String, Integer> statusCountMap) {
    if (statusCountMap.get(RecatConstants.PENDING) != 0) {
      return null;
    }
    recatProcess.setSuccessCount(statusCountMap.get(RecatConstants.FINISHED));
    recatProcess.setErrorCount(statusCountMap.get(RecatConstants.FAILED));
    recatProcess.setInputErrorCount(statusCountMap.get(RecatConstants.VALIDATION_ERROR));
    recatProcess.setSystemErrorCount(statusCountMap.get(RecatConstants.SYSTEM_ERROR));
    recatProcess.setEndTime(new Date());
    if (recatProcess.getTotalCount().equals(recatProcess.getSuccessCount())) {
      recatProcess.setStatus(RecatConstants.FINISHED);
    } else if (recatProcess.getTotalCount().equals(recatProcess.getErrorCount())) {
      recatProcess.setStatus(RecatConstants.FAILED);
    } else {
      recatProcess.setStatus(RecatConstants.PARTIAL_SUCCESS);
    }
    return recatProcessRepository.saveAndFlush(recatProcess);
  }

  @Override
  public void sendMailForFinishedRecatProcess(List<RecatProcess> recatProcessList) {
    Map<String, List<RecatProcess>> failureAndPartialSuccessMap = new HashMap<>();
    sendMailForSuccessAndGetFailureAndPartialSuccessMap(recatProcessList, failureAndPartialSuccessMap);
    sendMailForFailureAndPartialSuccess(failureAndPartialSuccessMap);
  }

  private void sendMailForFailureAndPartialSuccess(Map<String, List<RecatProcess>> failureAndPartialSuccessMap) {
    for (Map.Entry<String, List<RecatProcess>> recatProcesses : failureAndPartialSuccessMap.entrySet()) {
      List<RecatProcess> recatProcessesValue = recatProcesses.getValue();
      if (recatProcessesValue.size() == 1) {
        sendSingleRequestMail(recatProcessesValue);
      } else {
        sendMail(recatProcessesValue.get(0), getEmailParamsForFailures(recatProcessesValue),
            EmailConstants.RECAT_BULK_FAILURE, EmailConstants.RECAT_BULK_FAILURE_SUBJECT, null);
      }
    }
  }

  private void sendSingleRequestMail(List<RecatProcess> recatProcessesValue) {
    RecatProcess recatProcess = recatProcessesValue.get(0);
    Map<String, Object> emailParams = getEmailParams(recatProcess, null);
    if (RecatConstants.PARTIAL_SUCCESS.equals(recatProcess.getStatus())) {
      sendMail(recatProcess, emailParams, EmailConstants.RECAT_PARTIAL_SUCCESS,
          EmailConstants.RECAT_PARTIAL_SUCCESS_SUBJECT
              .replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()), null);
    } else if (RecatConstants.FAILED.equals(recatProcess.getStatus())) {
      sendMail(recatProcess, emailParams, EmailConstants.RECAT_FAILED, EmailConstants.RECAT_FAILED_SUBJECT
          .replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()), null);
    }
  }

  private void sendMailForSuccessAndGetFailureAndPartialSuccessMap(List<RecatProcess> recatProcessList,
      Map<String, List<RecatProcess>> recatProcessMap) {
    for (RecatProcess recatProcess : recatProcessList) {
      log.info("Sending mail for recat-request-code : {} ", recatProcess.getRecatRequestCode());
      if (RecatConstants.FINISHED.equals(recatProcess.getStatus())) {
        Map<String, Object> emailParams = getEmailParams(recatProcess, null);
        sendMail(recatProcess, emailParams, EmailConstants.RECAT_SUCCESS, EmailConstants.RECAT_SUCCESS_SUBJECT
            .replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()), null);
      } else {
        if (recatProcessMap.containsKey(recatProcess.getCreatedBy())) {
          List<RecatProcess> recatProcesses = new ArrayList<>(recatProcessMap.get(recatProcess.getCreatedBy()));
          recatProcesses.add(recatProcess);
          recatProcessMap.replace(recatProcess.getCreatedBy(), recatProcesses);
        } else {
          recatProcessMap.putIfAbsent(recatProcess.getCreatedBy(), Collections.singletonList(recatProcess));
        }
      }
    }
  }

  @Override
  public void sendMailForRecatFailedProducts(RecatProcess recatProcess, String mailTo){
    log.info("Sending mail for recat-request-code : {} ", recatProcess.getRecatRequestCode());
    Map<String, Object> emailParams = getEmailParams(recatProcess, mailTo);
    sendMail(recatProcess, emailParams, EmailConstants.RECAT_DOWNLOAD,
        EmailConstants.RECAT_DOWNLOAD_SUBJECT.replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()), mailTo);
  }

  private void sendMail(RecatProcess recatProcess, Map<String, Object> emailParams, String messageId,
      String emailSubject, String mailTo) {
    try {
      MessageEmailRequest email = new MessageEmailRequest();
      email.setMessageId(messageId);
      email.setMessageTo(Optional.ofNullable(mailTo).orElse(recatProcess.getCreatedBy()));
      email.setMessageFrom(EmailConstants.MAIL_SENDER);
      email.setMessageIdentifierKey(messageId);
      email.setMessageIdentifierValue(UUID.randomUUID().toString());
      email.setMessageSubject(emailSubject);
      email.setVariables(emailParams);
      log.info("Sending mail for recat-request-code : {} email : {} ", recatProcess.getRecatRequestCode(), email);
      ConverterUtil.setMandatoryParamsToMessageEmailRequest(email);
      kafkaProducer.send(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT,
        email.getMessageIdentifierValue(), email);
    } catch (Exception e) {
      log.error("exception sending mail for recat-request-code : {}. Error - ",
        recatProcess.getRecatRequestCode(), e);
    }
  }

  public Map<String, Object> getEmailParams(RecatProcess request, String mailTo) {
    Map<String, Object> emailParameters = new HashMap<>();
    emailParameters.put(EmailConstants.NAME, getUserName(Optional.ofNullable(mailTo).orElse(request.getCreatedBy())));
    emailParameters.put(EmailConstants.REQUEST_CODE, request.getRecatRequestCode());
    emailParameters.put(EmailConstants.TOTAL_COUNT, request.getTotalCount());
    emailParameters.put(EmailConstants.SUCCESS_COUNT, request.getSuccessCount());
    emailParameters.put(EmailConstants.FILE_PREFIX, fileStorageService.getEmailPrefix());
    return emailParameters;
  }

  private Map<String, Object> getEmailParamsForFailures(List<RecatProcess> request) {
    Map<String, Object> emailParameters = new HashMap<>();
    List<FailedRecatProcessResponse> failedRecatProcessResponses = getFailedRecatProcessResponses(request);
    Map<String, Object> mailObjectWrapper = new HashMap<>();
    mailObjectWrapper.put(EmailConstants.RECAT_PROCESS_LIST, failedRecatProcessResponses);
    mailObjectWrapper.put(EmailConstants.NAME, getUserName(request.get(0).getCreatedBy()));
    emailParameters.put(Constant.EMAIL_OBJECT, mailObjectWrapper);
    return emailParameters;
  }

  private List<FailedRecatProcessResponse> getFailedRecatProcessResponses(List<RecatProcess> request) {
    return request.stream().map(
        processList -> FailedRecatProcessResponse.builder().recatRequestCode(processList.getRecatRequestCode())
            .totalCount(processList.getTotalCount()).successCount(processList.getSuccessCount())
            .errorCount(processList.getErrorCount()).build()).collect(Collectors.toList());
  }

  @Override
  public RecatProcess findRecatProcessByRecatRequestCode(String storeId, String recatRequestCode) {
    return recatProcessRepository.findByStoreIdAndRecatRequestCode(storeId, recatRequestCode);
  }

  public String getUserName(String email) {
    return email.split(Constant.USER_NAME_SPLIT)[0];
  }

  @Override
  public void sendMailForNewRecatProcess(RecatProcess recatProcess) {
    Map<String, Object> emailParams = getEmailParams(recatProcess, null);
    sendMail(recatProcess, emailParams, EmailConstants.RECAT_CREATED,
        EmailConstants.RECAT_CREATED_SUBJECT.replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()), null);
  }

  @Override
  public void deleteRecatFile(String fileName) {
    try {
      ProcessorUtils.deleteFile(fileName);
    } catch (Exception e) {
      log.error("Error when deleting recat file : {} ", fileName, e);
    }
  }
}
