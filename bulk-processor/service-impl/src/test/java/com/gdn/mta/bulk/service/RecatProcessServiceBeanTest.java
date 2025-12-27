package com.gdn.mta.bulk.service;

import static org.mockito.MockitoAnnotations.initMocks;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.entity.RecatProcess;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.repository.RecatProcessRepository;
import com.gdn.partners.bulk.util.RecatConstants;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;

public class RecatProcessServiceBeanTest {

  private static final String STORE_ID = "10001";
  private static final String RECAT_REQUEST_CODE = "request-code";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final int TOTAL_RECORDS = 1;
  private static final String USER_NAME = "userName@gmail.com";

  private RecatProcess recatProcess;
  private Map<String, Integer> statusCountMap;
  private RecatProcessSummaryRequest recatProcessSummaryRequest = new RecatProcessSummaryRequest();

  @InjectMocks
  private RecatProcessServiceBean recatProcessService;

  @Mock
  private RecatProcessRepository recatProcessRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private FileStorageServiceBean fileStorageServiceBean;

  @Captor
  private ArgumentCaptor<RecatProcess> recatProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<MessageEmailRequest> emailRequestArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    recatProcess = new RecatProcess();
    recatProcess.setStatus(RecatConstants.NEW);
    recatProcess.setRecatRequestCode(RECAT_REQUEST_CODE);
    Date date = getDate(1);
    recatProcess.setScheduledTime(date);
    recatProcess.setCreatedBy(USER_NAME);

    statusCountMap = new HashMap<>();
    statusCountMap.putIfAbsent(RecatConstants.PENDING, 0);
    statusCountMap.putIfAbsent(RecatConstants.FINISHED, 100);
    statusCountMap.putIfAbsent(RecatConstants.FAILED, 50);
    statusCountMap.putIfAbsent(RecatConstants.VALIDATION_ERROR, 30);
    statusCountMap.putIfAbsent(RecatConstants.SYSTEM_ERROR, 20);
  }

  private Date getDate(int value) {
    Date date = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DATE, value);
    date = calendar.getTime();
    return date;
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(recatProcessRepository);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(fileStorageServiceBean);
  }

  @Test
  public void getAllNewRecatProcessTest() throws ParseException {
    recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(recatProcessRepository).findByStoreIdAndStatus(STORE_ID, RecatConstants.NEW);
  }

  @Test
  public void getAllNewRecatProcessScheduledDateTest() throws ParseException {
    recatProcess.setScheduledTime(new Date());
    Mockito.when(recatProcessRepository.findByStoreIdAndStatus(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    List<RecatProcess> recatProcess = recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(recatProcessRepository).findByStoreIdAndStatus(STORE_ID, RecatConstants.NEW);
    Assertions.assertEquals(1, recatProcess.size());
    Assertions.assertEquals(RECAT_REQUEST_CODE, recatProcess.get(0).getRecatRequestCode());
  }

  @Test
  public void getAllNewRecatProcessBeforeScheduledDateTest() throws ParseException {
    recatProcess.setScheduledTime(getDate(-2));
    Mockito.when(recatProcessRepository.findByStoreIdAndStatus(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    List<RecatProcess> recatProcess = recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(recatProcessRepository).findByStoreIdAndStatus(STORE_ID, RecatConstants.NEW);
    Assertions.assertEquals(1, recatProcess.size());
    Assertions.assertEquals(RECAT_REQUEST_CODE, recatProcess.get(0).getRecatRequestCode());
  }

  @Test
  public void getAllNewRecatProcessAfterScheduledDateTest() throws ParseException {
    recatProcess.setScheduledTime(getDate(2));
    Mockito.when(recatProcessRepository.findByStoreIdAndStatus(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    List<RecatProcess> recatProcess = recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(recatProcessRepository).findByStoreIdAndStatus(STORE_ID, RecatConstants.NEW);
    Assertions.assertEquals(0, recatProcess.size());
  }

  @Test
  public void getAllNewRecatProcessNoNewRequestsTest() throws ParseException {
    Mockito.when(recatProcessRepository.findByStoreIdAndStatus(STORE_ID, RecatConstants.NEW))
        .thenReturn(null);
    List<RecatProcess> recatProcess = recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(recatProcessRepository).findByStoreIdAndStatus(STORE_ID, RecatConstants.NEW);
    Assertions.assertEquals(0, recatProcess.size());
  }

  @Test
  public void saveRecatProcessTest() {
    recatProcessService.saveRecatProcess(new RecatProcess());
    Mockito.verify(recatProcessRepository).saveAndFlush(new RecatProcess());
  }

  @Test
  public void getRecatProcessSummaryTest() {
    Mockito.when(this.recatProcessRepository
        .findSummaryByFilter(STORE_ID, recatProcessSummaryRequest, PAGE, SIZE)).thenReturn(
        new PageImpl<>(Arrays.asList(recatProcess), PageRequest.of(PAGE, SIZE), TOTAL_RECORDS));
    Page<RecatProcess> response = recatProcessService
        .getRecatProcessSummary(STORE_ID, recatProcessSummaryRequest, PAGE, SIZE);
    Mockito.verify(this.recatProcessRepository)
        .findSummaryByFilter(STORE_ID, recatProcessSummaryRequest, PAGE, SIZE);
    Assertions.assertEquals(RECAT_REQUEST_CODE, response.getContent().get(0).getRecatRequestCode());
    Assertions.assertEquals(TOTAL_RECORDS, response.getTotalElements());
  }

  @Test
  public void getRecatProcessByStoreIdAndStatusTest() {
    recatProcessService.getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
    Mockito.verify(recatProcessRepository).findByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
  }

  @Test
  public void getRecatProcessByRecatRequestCodeTest() {
    recatProcessService.getRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessRepository).findByStoreIdAndRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
  }

  @Test
  public void updateFinalStatusPendingTest() {
    statusCountMap.replace(RecatConstants.PENDING, 10);
    RecatProcess recatProcess = recatProcessService.updateFinalStatus(this.recatProcess, statusCountMap);
    Assertions.assertNull(recatProcess);
  }

  @Test
  public void updateFinalStatusTest() {
    statusCountMap.replace(RecatConstants.FINISHED, 100);
    recatProcess.setTotalCount(100);
    recatProcessService.updateFinalStatus(this.recatProcess, statusCountMap);
    Mockito.verify(recatProcessRepository).saveAndFlush(recatProcessArgumentCaptor.capture());
    Assertions.assertEquals(RecatConstants.FINISHED, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(100, recatProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(100, recatProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(50, recatProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(30, recatProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(20, recatProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
  }

  @Test
  public void updateFinalStatusPartialTest() {
    statusCountMap.replace(RecatConstants.FAILED, 50);
    statusCountMap.replace(RecatConstants.FINISHED, 50);
    recatProcess.setTotalCount(100);
    recatProcessService.updateFinalStatus(this.recatProcess, statusCountMap);
    Mockito.verify(recatProcessRepository).saveAndFlush(recatProcessArgumentCaptor.capture());
    Assertions.assertEquals(RecatConstants.PARTIAL_SUCCESS, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(100, recatProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(50, recatProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(50, recatProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(30, recatProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(20, recatProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
  }

  @Test
  public void updateFinalStatusFailedTest() {
    statusCountMap.replace(RecatConstants.FAILED, 100);
    statusCountMap.replace(RecatConstants.FINISHED, 0);
    recatProcess.setTotalCount(100);
    recatProcessService.updateFinalStatus(this.recatProcess, statusCountMap);
    Mockito.verify(recatProcessRepository).saveAndFlush(recatProcessArgumentCaptor.capture());
    Assertions.assertEquals(RecatConstants.FAILED, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(100, recatProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, recatProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(100, recatProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Assertions.assertEquals(30, recatProcessArgumentCaptor.getValue().getInputErrorCount(), 0);
    Assertions.assertEquals(20, recatProcessArgumentCaptor.getValue().getSystemErrorCount(), 0);
  }

  @Test
  public void sendMailForFinishedRecatProcessTest() throws Exception {
    recatProcess.setStatus(RecatConstants.FINISHED);
    recatProcessService.sendMailForFinishedRecatProcess(Collections.singletonList(recatProcess));
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), emailRequestArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean).getEmailPrefix();
    Assertions.assertEquals(EmailConstants.RECAT_SUCCESS, emailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(EmailConstants.RECAT_SUCCESS, emailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(
        EmailConstants.RECAT_SUCCESS_SUBJECT.replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()),
        emailRequestArgumentCaptor.getValue().getMessageSubject());
  }

  @Test
  public void sendMailForFinishedRecatProcessPartialTest() throws Exception {
    recatProcess.setStatus(RecatConstants.PARTIAL_SUCCESS);
    recatProcessService.sendMailForFinishedRecatProcess(Collections.singletonList(recatProcess));
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), emailRequestArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean).getEmailPrefix();
    Assertions.assertEquals(EmailConstants.RECAT_PARTIAL_SUCCESS, emailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(EmailConstants.RECAT_PARTIAL_SUCCESS,
        emailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(EmailConstants.RECAT_PARTIAL_SUCCESS_SUBJECT
            .replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()),
        emailRequestArgumentCaptor.getValue().getMessageSubject());
  }

  @Test
  public void sendMailForFinishedRecatProcessFailedTest1() throws Exception {
    recatProcess.setStatus(RecatConstants.FAILED);
    recatProcessService.sendMailForFinishedRecatProcess(Collections.singletonList(recatProcess));
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), emailRequestArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean).getEmailPrefix();
    Assertions.assertEquals(EmailConstants.RECAT_FAILED, emailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(EmailConstants.RECAT_FAILED, emailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(
        EmailConstants.RECAT_FAILED_SUBJECT.replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()),
        emailRequestArgumentCaptor.getValue().getMessageSubject());
  }

  @Test
  public void sendMailForFinishedRecatProcessFailedTest() throws Exception {
    RecatProcess recatProcess1 = new RecatProcess();
    recatProcess1.setStatus(RecatConstants.PARTIAL_SUCCESS);
    recatProcess1.setRecatRequestCode(RECAT_REQUEST_CODE);
    Date date = getDate(1);
    recatProcess1.setScheduledTime(date);
    recatProcess1.setCreatedBy(USER_NAME);
    List<RecatProcess> processes = new ArrayList<>();
    recatProcess.setStatus(RecatConstants.FAILED);
    recatProcess1.setTotalCount(1);
    recatProcess1.setErrorCount(0);
    recatProcess1.setSuccessCount(1);
    recatProcess.setTotalCount(1);
    recatProcess.setErrorCount(0);
    recatProcess.setSuccessCount(1);
    processes.add(recatProcess1);
    processes.add(recatProcess);
    recatProcessService.sendMailForFinishedRecatProcess(processes);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), emailRequestArgumentCaptor.capture());
    Assertions.assertEquals(EmailConstants.RECAT_BULK_FAILURE, emailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(EmailConstants.RECAT_BULK_FAILURE,
        emailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(EmailConstants.RECAT_BULK_FAILURE_SUBJECT,
        emailRequestArgumentCaptor.getValue().getMessageSubject());
  }

  @Test
  public void sendMailExceptionTest() throws Exception {
    recatProcess.setStatus(RecatConstants.FINISHED);
    recatProcessService.sendMailForFinishedRecatProcess(Collections.singletonList(recatProcess));
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), emailRequestArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean).getEmailPrefix();
    Assertions.assertEquals(EmailConstants.RECAT_SUCCESS, emailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(EmailConstants.RECAT_SUCCESS, emailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(
        EmailConstants.RECAT_SUCCESS_SUBJECT.replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()),
        emailRequestArgumentCaptor.getValue().getMessageSubject());
  }

  @Test
  public void getRecatProcessByStoreIdAndRecatRequestCodeTest() {
    recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessRepository).findByStoreIdAndRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
  }

  @Test
  public void findRecatProcessByRecatRequestCodeTest() {
    Mockito.when(recatProcessRepository.findByStoreIdAndRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(recatProcess);
    RecatProcess process = recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessRepository).findByStoreIdAndRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Assertions.assertNotNull(process);
  }

  @Test
  public void sendMailForNewRecatProcessTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(
      kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), emailRequestArgumentCaptor.capture());
    recatProcessService.sendMailForNewRecatProcess(recatProcess);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), emailRequestArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean).getEmailPrefix();
    Assertions.assertEquals(EmailConstants.RECAT_CREATED, emailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(EmailConstants.RECAT_CREATED, emailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(
        EmailConstants.RECAT_CREATED_SUBJECT.replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()),
        emailRequestArgumentCaptor.getValue().getMessageSubject());
  }

  @Test
  public void sendMailForRecatFailedProductsTest() throws Exception {
    recatProcess.setStatus(RecatConstants.FINISHED);
    recatProcessService.sendMailForRecatFailedProducts(recatProcess, null);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), emailRequestArgumentCaptor.capture());
    Mockito.verify(fileStorageServiceBean).getEmailPrefix();
    Assertions.assertEquals(EmailConstants.RECAT_DOWNLOAD, emailRequestArgumentCaptor.getValue().getMessageId());
    Assertions.assertEquals(EmailConstants.RECAT_DOWNLOAD, emailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(
        EmailConstants.RECAT_DOWNLOAD_SUBJECT.replace(EmailConstants.REQUEST_CODE, recatProcess.getRecatRequestCode()),
        emailRequestArgumentCaptor.getValue().getMessageSubject());
  }

  @Test
  public void deleteRecatFileTest() {
    recatProcessService.deleteRecatFile(RecatConstants.PRODUCT_NAME);
  }

}