package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.dto.BulkDownloadMailRecipient;
import com.gdn.mta.bulk.dto.BulkPendingProductResponse;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MailDeliveryServiceImplTest {
  private static final String SEND_MAIL = "temp@gmail.com";
  private static final String BULK_PROCESS_TYPE = "bulkProcess";
  private static final String BUSINESS_PARTNER_CODE = "CODE";
  private static final String START_DATE = "START_DATE";
  private static final String MAIL_TEMPLATE_ID = "mailTemplateId";
  private static final String SENDER = "sender";
  private static final String USERNAME = "userName";
  private static final String SUBJECT = "SUBJECT";
  private static final String EMAIL_TO = "email_to@mail.com";
  private static final String EMAIL_CC = "email_cc@mail.com";
  private static final String EMAIL_CC_MULTIPLE =
      "email_cc@mail.com;email_cc_1@mail.com;email_cc_2@mail.com";
  private static final String MESSAGE_IDENTIFIER_KEY = "messageIdentifierKey";
  private static final String MESSAGE_IDENTIFIER_VALUE = "messageIdentifierValue";
  private static final String ABORTED_TIME = "30";

  private BulkDownloadMailRecipient bulkDownloadMailRecipient =
      new BulkDownloadMailRecipient(EMAIL_CC, EMAIL_TO);
  private Map<String, Object> templateParams = new HashMap<>();
  private BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
  private BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
  private BulkDownloadRequest.BulkRequestBuilder bulkRequestBuilder =
      new BulkDownloadRequest.BulkRequestBuilder();
  private Map<String, Object> emailParams = new HashMap<>();

  private BulkPendingProductResponse bulkPendingProductResponse;
  private List<BulkPendingProductResponse> bulkPendingProductResponseList;

  @Mock
  private KafkaPublisher kafkaProducer;

  @InjectMocks
  private MailDeliveryServiceImpl mailDeliveryService;

  @Captor
  private ArgumentCaptor<MessageEmailRequest> messageEmailRequestArgumentCaptor;

  @BeforeEach
  public void initializeTest(){
    MockitoAnnotations.initMocks(this);
    bulkPendingProductResponseList = new ArrayList<>();
    bulkPendingProductResponse = new BulkPendingProductResponse();
    bulkPendingProductResponse.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkPendingProductResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    bulkPendingProductResponse.setStartDate(START_DATE);
    bulkPendingProductResponseList.add(bulkPendingProductResponse);
    bulkDownloadRequest = bulkRequestBuilder.emailTo(EMAIL_TO).emailCC(EMAIL_CC).username(USERNAME).build();
    emailParams.put(EmailConstants.TEMPLATE_ID_PARAM, MAIL_TEMPLATE_ID);
    emailParams.put(EmailConstants.MAIL_SENDER_PARAM, SENDER);
    emailParams.put(EmailConstants.MAIL_SUBJECT_PARAM, SUBJECT);
  }

  @AfterEach
  public void finalizeTest(){
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  public void sendPendingBulkRequestMailTest() throws Exception{
    this.mailDeliveryService.sendPendingBulkRequestMail(SEND_MAIL, bulkPendingProductResponseList,
        bulkPendingProductResponseList, bulkPendingProductResponseList, ABORTED_TIME);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(SEND_MAIL, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertEquals(EmailConstants.PENDING_REQUESTS_TEMPLATE_ID, messageEmailRequestArgumentCaptor.getValue().getMessageId());
  }

  @Test
  public void sendPendingBulkRequestMailFailedTest() throws Exception{
    try {
      Mockito.doThrow(ApplicationRuntimeException.class).when(kafkaProducer).send(Mockito.anyString(),
          Mockito.anyString(), Mockito.any(MessageEmailRequest.class));
      Assertions.assertThrows(RuntimeException.class,
          () -> this.mailDeliveryService.sendPendingBulkRequestMail(SEND_MAIL,
              bulkPendingProductResponseList, bulkPendingProductResponseList,
              bulkPendingProductResponseList, ABORTED_TIME));
    } finally {
      Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
      Assertions.assertEquals(SEND_MAIL, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
      Assertions.assertEquals(EmailConstants.PENDING_REQUESTS_TEMPLATE_ID,
          messageEmailRequestArgumentCaptor.getValue().getMessageId());
    }
  }

  @Test
  public void sendBulkDownloadEmailTest() throws Exception {
    mailDeliveryService.sendBulkDownloadEmail(MAIL_TEMPLATE_ID, SENDER, SUBJECT, templateParams,
        MESSAGE_IDENTIFIER_KEY, MESSAGE_IDENTIFIER_VALUE, bulkDownloadMailRecipient);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(EMAIL_TO, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertEquals(EMAIL_CC, messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertEquals(MESSAGE_IDENTIFIER_KEY,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(MESSAGE_IDENTIFIER_VALUE,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierValue());
  }

  @Test
  public void sendBulkDownloadEmail_semicolonSeparatedCCTest() throws Exception {
    bulkDownloadMailRecipient.setEmailCc(EMAIL_CC_MULTIPLE);
    mailDeliveryService.sendBulkDownloadEmail(MAIL_TEMPLATE_ID, SENDER, SUBJECT, templateParams,
        MESSAGE_IDENTIFIER_KEY, MESSAGE_IDENTIFIER_VALUE, bulkDownloadMailRecipient);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(EMAIL_TO, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertEquals(EMAIL_CC_MULTIPLE, messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertEquals(MESSAGE_IDENTIFIER_KEY,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(MESSAGE_IDENTIFIER_VALUE,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierValue());
  }

  @Test
  public void sendBulkDownloadEmail_invalidEmailCCTest() throws Exception {
    bulkDownloadMailRecipient.setEmailCc(MESSAGE_IDENTIFIER_KEY);
    mailDeliveryService.sendBulkDownloadEmail(MAIL_TEMPLATE_ID, SENDER, SUBJECT, templateParams,
        MESSAGE_IDENTIFIER_KEY, MESSAGE_IDENTIFIER_VALUE, bulkDownloadMailRecipient);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(EMAIL_TO, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertNull(messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertEquals(MESSAGE_IDENTIFIER_KEY,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(MESSAGE_IDENTIFIER_VALUE,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierValue());
  }

  @Test
  public void sendBulkDownloadEmail_emptyTemplateIdTest() throws Exception {
    Assertions.assertThrows(RuntimeException.class,
        () -> mailDeliveryService.sendBulkDownloadEmail(StringUtils.EMPTY, SENDER, SUBJECT, templateParams,
        MESSAGE_IDENTIFIER_KEY, MESSAGE_IDENTIFIER_VALUE, bulkDownloadMailRecipient));
  }

  @Test
  public void sendEmailTest() throws Exception {
    mailDeliveryService.sendEmail(bulkDownloadRequest, emailParams);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(EMAIL_TO, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertEquals(EMAIL_CC, messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertEquals(USERNAME,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(USERNAME,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierValue());
  }

  @Test
  public void sendEmailTest_emptyEmailCc() throws Exception {
    bulkDownloadRequest = bulkRequestBuilder.username(USERNAME).emailTo(EMAIL_TO).emailCC(null).build();
    mailDeliveryService.sendEmail(bulkDownloadRequest, emailParams);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(EMAIL_TO, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertNull(messageEmailRequestArgumentCaptor.getValue().getMessageCc());
    Assertions.assertEquals(USERNAME,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierKey());
    Assertions.assertEquals(USERNAME,
        messageEmailRequestArgumentCaptor.getValue().getMessageIdentifierValue());
  }

  @Test
  public void sendEmailByForInternalBulkProcessTest() throws Exception {
    bulkInternalProcess.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcess.setInternalProcessRequestCode(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcess.setCreatedBy(SEND_MAIL);
    this.mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess, ProcessStatus.PENDING.name());
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(SEND_MAIL, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertEquals(EmailConstants.STORE_COPY_CREATION, messageEmailRequestArgumentCaptor.getValue().getMessageId());
  }

  @Test
  public void sendEmailByForInternalBulkProcessMasterSkuReviewTest() throws Exception {
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    bulkInternalProcess.setInternalProcessRequestCode(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    bulkInternalProcess.setCreatedBy(SEND_MAIL);
    this.mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess, ProcessStatus.FAILED.name());
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(SEND_MAIL, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertEquals(EmailConstants.BULK_MASTER_SKU_REVIEW_FAILED_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
  }

  @Test
  public void sendEmailByForInternalBulkProcessMasterSkuAssigneeTest() throws Exception {
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    bulkInternalProcess.setInternalProcessRequestCode(BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    bulkInternalProcess.setCreatedBy(SEND_MAIL);
    this.mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess, ProcessStatus.FAILED.name());
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(SEND_MAIL, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertEquals(EmailConstants.MASTER_SKU_BULK_ASSIGNEE_FAILED_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
  }

  @Test
  public void sendEmailByForInternalBulkProcessSalesCategoryUpdateTest() throws Exception {
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    bulkInternalProcess.setInternalProcessRequestCode(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name());
    bulkInternalProcess.setCreatedBy(SEND_MAIL);
    this.mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess, ProcessStatus.PENDING.name());
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(SEND_MAIL, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertEquals(EmailConstants.UPDATE_SALES_CATEGORY_CREATION, messageEmailRequestArgumentCaptor.getValue().getMessageId());
  }

  @Test
  public void sendEmailByForInternalBulkProcessExceptionTest() throws Exception {
    bulkInternalProcess.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcess.setInternalProcessRequestCode(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcess.setCreatedBy(SEND_MAIL);
    this.mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess, ProcessStatus.PENDING.name());
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
        Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
  }

  @Test
  public void sendEmailByForInternalBulkProcessException1Test() throws Exception {
    bulkInternalProcess.setProcessType(SEND_MAIL);
    bulkInternalProcess.setInternalProcessRequestCode(BulkInternalProcessType.STORE_COPY.name());
    bulkInternalProcess.setCreatedBy(SEND_MAIL);
    Assertions.assertThrows(RuntimeException.class,
        () -> this.mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess,
            ProcessStatus.PENDING.name()));
  }

  @Test
  public void sendEmailByForInternalBulkProcessIPRProductsAddReviewTest() throws Exception {
    bulkInternalProcess.setProcessType(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    bulkInternalProcess.setInternalProcessRequestCode(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    bulkInternalProcess.setCreatedBy(SEND_MAIL);
    this.mailDeliveryService.sendEmailByForInternalBulkProcess(bulkInternalProcess, ProcessStatus.FAILED.name());
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        messageEmailRequestArgumentCaptor.capture());
    Assertions.assertEquals(SEND_MAIL, messageEmailRequestArgumentCaptor.getValue().getMessageTo());
    Assertions.assertEquals(EmailConstants.BULK_IPR_PRODUCT_ADD_REVIEW_FAILED_TEMPLATE_ID,
        messageEmailRequestArgumentCaptor.getValue().getMessageId());
  }
}
