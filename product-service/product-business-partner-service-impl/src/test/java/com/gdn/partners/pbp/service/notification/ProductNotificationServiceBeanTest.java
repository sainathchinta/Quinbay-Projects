package com.gdn.partners.pbp.service.notification;

import static com.gdn.mta.notification.enumeration.NotificationType.PRODUCT_STATUS;
import static com.gdn.mta.notification.enumeration.NotificationType.RESTRICTED_PRODUCT_ACTIVATED;
import static org.mockito.Mockito.verify;

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
import org.slf4j.MDC;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.partners.kafka.notification.NotificationKafka;
import com.gdn.partners.pbp.entity.mailEvent.CategoryChangeMailEvent;

public class ProductNotificationServiceBeanTest {

  private static final String BUSINESS_PARTNER_CODE = "BP-CODE";
  private static final String GDN_SKU = "TOS-12345";
  private static final Integer PRODUCT_COUNT = 5;
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String REQUEST_ID = "REQ_ID";
  private static final String USERNAME = "USERNAME";
  private static final String CATEGORY_CHANGE_NOTES = "Wrong Category-Add-Test";
  private static final String NOTES = "NOTES-Add";
  private static final String NOTES_WITH_MULTIPLE_REASONS = "Brand Incorrect,Content incomplete or wrong,Image wrong or bad quality,Variant should be combined,Wrong Category-Add";
  private static final String FILEPATH = "filepath";
  private static final String URL_SOURCE = "https://static-src-qa/";
  private static final String PRODUCT_SUCCESSFULLY_ADDED_AS_FBB = "All product successfully added as FBB.";
  private static final String STORE_ID = "10001";
  private static final String MANDATORY_PARAMETER = "mandatoryParameter";
  @Captor
  private ArgumentCaptor<NotificationKafka> notificationCaptor;

  @InjectMocks
  private ProductNotificationServiceBean service;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    GdnBaseRestResponse successResponse = new GdnBaseRestResponse();
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getUsername()).thenReturn(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    successResponse.setSuccess(true);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(applicationProperties);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  public void sendProductActiveNotificationTest() throws Exception {
    service.sendProductActiveNotification(BUSINESS_PARTNER_CODE, GDN_SKU);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), Mockito.any(NotificationKafka.class));
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductActiveNotificationTracerNullTest() throws Exception {
    service.sendProductActiveNotification(BUSINESS_PARTNER_CODE, GDN_SKU);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), Mockito.any(NotificationKafka.class));
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductStockMinimalNotificationTest() throws Exception {
    service.sendProductStockMinimalNotification(BUSINESS_PARTNER_CODE, PRODUCT_COUNT);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), Mockito.any(NotificationKafka.class));
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MINIMUM_STOCK_NOTIFICATION), Mockito.any());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductStockOosNotificationTest() throws Exception {
    service.sendProductStockOosNotification(BUSINESS_PARTNER_CODE, PRODUCT_COUNT);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_OOS_STOCK_NOTIFICATION), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), Mockito.any(NotificationKafka.class));
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductRejectNotificationTest() throws Exception {
    service.sendProductRejectNotification(BUSINESS_PARTNER_CODE, PRODUCT_NAME);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    NotificationKafka request = notificationCaptor.getValue();
    Assertions.assertEquals(request.getNotificationType(), NotificationType.PRODUCT_REJECTED.getValue());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductReturnForCorrectionNotificationTest() throws Exception {
    service.sendProductReturnForCorrectionNotification(BUSINESS_PARTNER_CODE, PRODUCT_NAME, GDN_SKU,
        NOTES);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    NotificationKafka request = notificationCaptor.getValue();
    Assertions.assertEquals(request.getNotificationType(),
        NotificationType.PRODUCT_SEND_FOR_CORRECTION.getValue());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductReturnForCorrectionNotificationCategoryTest() throws Exception {
    service.sendProductReturnForCorrectionNotification(BUSINESS_PARTNER_CODE, PRODUCT_NAME, GDN_SKU,
        CATEGORY_CHANGE_NOTES);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    NotificationKafka request = notificationCaptor.getValue();
    Assertions.assertEquals(request.getNotificationType(),
        NotificationType.PRODUCT_SEND_FOR_CORRECTION_CATEGORY_CHANGE.getValue());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductReturnForCorrectionNotificationCategoryWithMultipleReasonsTest() throws Exception {
    service.sendProductReturnForCorrectionNotification(BUSINESS_PARTNER_CODE, PRODUCT_NAME, GDN_SKU,
        NOTES_WITH_MULTIPLE_REASONS);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    NotificationKafka request = notificationCaptor.getValue();
    Assertions.assertEquals(request.getNotificationType(),
        NotificationType.PRODUCT_SEND_FOR_CORRECTION_CATEGORY_CHANGE.getValue());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductActiveNotificationTest_WithCategoryChange() throws Exception {
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    service.sendProductActiveNotification(BUSINESS_PARTNER_CODE, categoryChangeMailEvent, GDN_SKU, false);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductActiveNotificationTest_WithName() throws Exception {
    service.sendProductActiveNotification(BUSINESS_PARTNER_CODE, GDN_SKU, "", false);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductQRCodeDownloadNotificationTest() throws Exception {
    Mockito.when(applicationProperties.getQRCodeUrlSource()).thenReturn(URL_SOURCE);
    service.sendProductQRCodeDownloadNotification(BUSINESS_PARTNER_CODE, FILEPATH);
    Mockito.verify(applicationProperties).getQRCodeUrlSource();
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductSyncNotificationTest_whenCompleteSuccess() throws Exception {
    service.sendProductSyncNotification(BUSINESS_PARTNER_CODE, PRODUCT_SUCCESSFULLY_ADDED_AS_FBB);

    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());

    NotificationKafka request = notificationCaptor.getValue();
    Assertions.assertEquals(request.getNotificationType(),
        NotificationType.PRODUCT_SYNC_ACTIVITY.getValue());
    Assertions.assertEquals(request.getNotificationMessage(), PRODUCT_SUCCESSFULLY_ADDED_AS_FBB);
    Assertions.assertEquals(request.getNotificationDetail(), "/external/product/active/all");
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendNotificationForProductWithRestrictedKeywordTest() throws Exception {
    service.sendNotificationForProductWithRestrictedKeyword(BUSINESS_PARTNER_CODE, PRODUCT_NAME, Boolean.FALSE);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    Assertions.assertEquals(RESTRICTED_PRODUCT_ACTIVATED.getValue(), notificationCaptor.getValue().getNotificationType());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendNotificationForProductStatusTest() throws Exception {
    service.sendNotificationForProductStatus(BUSINESS_PARTNER_CODE, PRODUCT_NAME, ProductLevel3Status.ONLINE.name());
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    Assertions.assertEquals(PRODUCT_STATUS.getValue(), notificationCaptor.getValue().getNotificationType());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendNotificationForProductStatus_TeaserTest() throws Exception {
    service.sendNotificationForProductStatus(BUSINESS_PARTNER_CODE, PRODUCT_NAME, ProductLevel3Status.TEASER.name());
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    Assertions.assertEquals(PRODUCT_STATUS.getValue(), notificationCaptor.getValue().getNotificationType());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendNotificationForProductStatus_B2BTest() throws Exception {
    service.sendNotificationForProductStatus(BUSINESS_PARTNER_CODE, PRODUCT_NAME, ProductLevel3Status.B2B.name());
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), notificationCaptor.capture());
    Assertions.assertEquals(PRODUCT_STATUS.getValue(), notificationCaptor.getValue().getNotificationType());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }
}
