package com.gdn.x.productcategorybase.service.mailservice;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;

import com.gdn.x.productcategorybase.domain.event.model.BrandAuthorisationWipActionEventModel;
import com.gdn.x.productcategorybase.domain.event.model.NearExpiryModelEvent;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.mail.MailRecipientRequest;

;

/**
 * Created by Vishal on 07/05/18.
 */
public class MailDeliveryServiceImplTest {

  private static final String DEFAULT_TEMPLATE = "DEFAULT_TEMPLATE";
  private static final String DEFAULT_SUBJECT = "DEFAULT_SUBJECT";
  private static final String VALID_EMAIL = "test.valid@gdn-commerce.com";
  private static final String STORE_ID = "10001";
  private static final String BUSINESS_PARTNER_CODE = "BP123";
  private static final String BRAND_AUTH_EMAIL_TYPE = "brand-auth-email-type";
  private static final String APPROVE = "APPROVE";
  private static final String REJECT = "REJECT";
  private static final String NEED_REVISION = "NEED_REVISION";
  private static final String NEAR_EXPIRY = "NEAR_EXPIRY";
  private static final String UNKNOWN = "UNKNOWN";
  private static final String ID = "ID";
  private static final String STATUS = "STATUS";
  private static final String BRAND_NAME = "BRAND";

  private MailRecipientRequest mailRecipientRequest;

  @InjectMocks
  private MailDeliveryServiceImpl mailDeliveryService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    when(kafkaTopicProperties.getBrandAuthApproveEmailEvent()).thenReturn("approve-topic");
    when(kafkaTopicProperties.getBrandAuthRejectEmailEvent()).thenReturn("reject-topic");
    when(kafkaTopicProperties.getBrandAuthNeedRevisionEmailEvent()).thenReturn("revision-topic");
    when(kafkaTopicProperties.getBrandAuthNearExpiryEmailEvent()).thenReturn("near-expiry-topic");

  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  public void sendMail_success() throws Exception {
    mailRecipientRequest = new MailRecipientRequest(VALID_EMAIL, VALID_EMAIL);
    mailDeliveryService
        .sendMail(DEFAULT_TEMPLATE, VALID_EMAIL, DEFAULT_SUBJECT, new HashMap<String, Object>(),
            StringUtils.EMPTY, StringUtils.EMPTY, mailRecipientRequest);
    Mockito.verify(kafkaProducer).send(eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendMail_successNonNullStoreId() throws Exception {
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    mailRecipientRequest = new MailRecipientRequest(VALID_EMAIL, VALID_EMAIL);
    mailDeliveryService
        .sendMail(DEFAULT_TEMPLATE, VALID_EMAIL, DEFAULT_SUBJECT, new HashMap<String, Object>(),
            StringUtils.EMPTY, StringUtils.EMPTY, mailRecipientRequest);
    Mockito.verify(kafkaProducer).send(eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendMail_success_withoutMailCc() throws Exception {
    mailRecipientRequest = new MailRecipientRequest(StringUtils.EMPTY, VALID_EMAIL);
    mailDeliveryService
        .sendMail(DEFAULT_TEMPLATE, VALID_EMAIL, DEFAULT_SUBJECT, new HashMap<String, Object>(),
            StringUtils.EMPTY, StringUtils.EMPTY, mailRecipientRequest);
    Mockito.verify(kafkaProducer).send(eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendMail_fail_EmptyEmail() throws Exception {
    mailRecipientRequest = new MailRecipientRequest(StringUtils.EMPTY, StringUtils.EMPTY);
    mailDeliveryService
        .sendMail(DEFAULT_TEMPLATE, VALID_EMAIL, DEFAULT_SUBJECT, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, mailRecipientRequest);
  }

  @Test
  public void sendMail_fail_EmptyTemplateId() throws Exception {
    mailRecipientRequest = new MailRecipientRequest(StringUtils.EMPTY, VALID_EMAIL);
    mailDeliveryService
        .sendMail(StringUtils.EMPTY, VALID_EMAIL, DEFAULT_SUBJECT, new HashMap<>(),
            StringUtils.EMPTY, StringUtils.EMPTY, mailRecipientRequest);
  }

  @Test
  public void testSendBrandAuthorisationActionMail_Approve() {
    BrandAuthorisationWipActionEventModel expectedEventModel =
        BrandAuthorisationWipActionEventModel.builder().notificationType(BRAND_AUTH_EMAIL_TYPE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).status(STATUS).id(ID)
            .nearExpiryModelEvents(new ArrayList<>()).build();
    mailDeliveryService.sendBrandAuthorisationActionMail(BUSINESS_PARTNER_CODE, BRAND_AUTH_EMAIL_TYPE,
        APPROVE, STATUS, ID, new ArrayList<>());
    verify(kafkaProducer).send(eq("approve-topic"), eq(BUSINESS_PARTNER_CODE),
        eq(expectedEventModel));
  }

  @Test
  public void testSendBrandAuthorisationActionMail_Reject() {
    BrandAuthorisationWipActionEventModel expectedEventModel =
        BrandAuthorisationWipActionEventModel.builder().notificationType(BRAND_AUTH_EMAIL_TYPE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).status(STATUS).id(ID)
            .nearExpiryModelEvents(new ArrayList<>()).build();
    mailDeliveryService.sendBrandAuthorisationActionMail(BUSINESS_PARTNER_CODE, BRAND_AUTH_EMAIL_TYPE,
        REJECT, STATUS, ID, new ArrayList<>());
    verify(kafkaProducer).send(eq("reject-topic"), eq(BUSINESS_PARTNER_CODE), eq(expectedEventModel));
  }

  @Test
  public void testSendBrandAuthorisationActionMail_NeedRevision() {
    BrandAuthorisationWipActionEventModel expectedEventModel =
        BrandAuthorisationWipActionEventModel.builder().notificationType(BRAND_AUTH_EMAIL_TYPE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).status(STATUS).id(ID)
            .nearExpiryModelEvents(new ArrayList<>()).build();
    mailDeliveryService.sendBrandAuthorisationActionMail(BUSINESS_PARTNER_CODE, BRAND_AUTH_EMAIL_TYPE,
        NEED_REVISION, STATUS, ID, new ArrayList<>());
    verify(kafkaProducer).send(eq("revision-topic"), eq(BUSINESS_PARTNER_CODE),
        eq(expectedEventModel));
  }

  @Test
  public void testSendBrandAuthorisationActionMail_NearExpiry() {
    NearExpiryModelEvent nearExpiryModelEvent = new NearExpiryModelEvent();
    nearExpiryModelEvent.setBrandName(BRAND_NAME);
    nearExpiryModelEvent.setAuthExpiryDate(new Date());
    BrandAuthorisationWipActionEventModel expectedEventModel =
        BrandAuthorisationWipActionEventModel.builder().notificationType(BRAND_AUTH_EMAIL_TYPE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).status(STATUS).id(ID)
            .nearExpiryModelEvents(Collections.singletonList(nearExpiryModelEvent)).build();
    mailDeliveryService.sendBrandAuthorisationActionMail(BUSINESS_PARTNER_CODE, BRAND_AUTH_EMAIL_TYPE,
        NEAR_EXPIRY, STATUS, ID, Collections.singletonList(nearExpiryModelEvent));
    verify(kafkaProducer).send(eq("near-expiry-topic"), eq(BUSINESS_PARTNER_CODE),
        eq(expectedEventModel));
  }

  @Test
  public void testSendBrandAuthorisationActionMail_UnknownAction() {
    mailDeliveryService.sendBrandAuthorisationActionMail(BUSINESS_PARTNER_CODE, BRAND_AUTH_EMAIL_TYPE,
          UNKNOWN, STATUS, ID, new ArrayList<>());
    verifyNoInteractions(kafkaProducer);
  }
}
