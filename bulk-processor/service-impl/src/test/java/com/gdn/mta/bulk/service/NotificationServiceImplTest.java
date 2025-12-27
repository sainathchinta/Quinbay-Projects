package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletion;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.GenerateQrCodeNotificationModel;
import com.gdn.partners.bulk.util.NotificationTypeConstant;
import com.gdn.x.neo.order.client.sdk.web.model.request.DateRangeRequest;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;
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
import org.slf4j.MDC;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.ApplicationProperties;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.OrderDownloadRequest;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.kafka.notification.NotificationKafka;
import org.springframework.test.util.ReflectionTestUtils;

public class NotificationServiceImplTest {

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private FileStorageService fileStorageService;

  @Captor
  private ArgumentCaptor<NotificationKafka> notificationKafkaArgumentCaptor;


  @Captor
  private ArgumentCaptor<GenerateQrCodeNotificationModel> generateQrCodeNotificationModelArgumentCaptor;

  @InjectMocks
  private NotificationServiceImpl notificationService;

  private BulkProcess bulkProcess;
  private BulkDownloadRequest bulkDownloadRequest;
  private BulkDownloadEntity bulkDownloadEntity;
  private OrderDownloadRequest orderDownloadRequest = new OrderDownloadRequest();
  private OrderItemSummaryRequest orderItemSummaryRequest = new OrderItemSummaryRequest();
  private DateRangeRequest dateRangeRequest = new DateRangeRequest();

  private static final String DESCRIPTION = "description";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String TOPIC_NAME = "topicName";
  private static final String MERCHANT_ID = "merchantId";
  private static final String FAILED_LINK = "failureLink";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String STORE_ID = "storeId";
  private static final String VENDOR_CODE = "vendorCode";
  private final DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
  private static final String FILE_PATH = "filePath";
  private static final String PRODUCT_SKU = "productSku";


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkProcess = new BulkProcess();
    bulkProcess.setDescription(DESCRIPTION);
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBusinessPartnerCode(MERCHANT_CODE);
    bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.VENDOR_FILTERED_PRODUCT);
    bulkDownloadRequest.setMerchantId(MERCHANT_ID);
    bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setBusinessPartnerCode(MERCHANT_CODE);
    Mockito.when(applicationProperties.getWebNotifNotificationCreateTopic()).thenReturn(TOPIC_NAME);
    Mockito.when(applicationProperties.getGenerateQrCodePushNotificationTopic()).thenReturn(TOPIC_NAME);
    Mockito.when(applicationProperties.getFailureQrCodePushNotificationTopic()).thenReturn(TOPIC_NAME);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(applicationProperties);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  public void sendNotificationSetNotificationDetailFalse() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    notificationService.sendNotification(bulkProcess, NotificationType.BULK_UPDATED.getValue(), false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE), 
        notificationKafkaArgumentCaptor.capture());
  }

  @Test
  public void sendNotificationSetNotificationDetailTrue() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, StringUtils.EMPTY);
    notificationService.sendNotification(bulkProcess, NotificationType.BULK_UPDATED.getValue(), true);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(BULK_PROCESS_CODE, notificationKafkaArgumentCaptor.getValue().getNotificationDetail());
  }

  @Test
  public void sendNotificationWhenDestinationKeyEmpty() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    bulkProcess.setBusinessPartnerCode(null);
    notificationService.sendNotification(bulkProcess, NotificationType.BULK_UPDATED.getValue(), false);
    Mockito.verify(kafkaProducer, times(0)).send(anyString(), anyString(), Mockito.any(NotificationKafka.class));
  }

  @Test
  public void sendBulkAssignNotificationAllSuccess() {
    notificationService.sendBulkAssignNotification(bulkProcess, 2, 2, VENDOR_CODE);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(Constant.ALL_PRODUCTS_SUCCESSFULLY_ASSIGNED,
        notificationKafkaArgumentCaptor.getValue().getNotificationMessage());
  }

  @Test
  public void sendBulkAssignNotificationWhenDestinationKeyEmpty() {
    notificationService.sendBulkAssignNotification(bulkProcess, 2, 2, null);
    Mockito.verify(kafkaProducer, times(0)).send(anyString(), anyString(), Mockito.any(NotificationKafka.class));
  }

  @Test
  public void sendBulkAssignNotificationWithFailures() {
    notificationService.sendBulkAssignNotification(bulkProcess, 1, 2, VENDOR_CODE);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationMessage()
        .contains(Constant.FEW_PRODUCTS_SUCCESSFULLY_ASSIGNED));
  }

  @Test
  public void sendNotificationForPromoAndWholesale() {
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    notificationService.sendNotificationWithErrorFileGenerated(
        bulkProcess, DESCRIPTION, true, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(BULK_PROCESS_CODE, notificationKafkaArgumentCaptor.getValue().getNotificationDetail());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationType()
        .contains(NotificationType.BULK_UPDATED_WITH_PROMO.getValue()));
  }

  @Test
  public void sendNotificationForPromoAndWholesalePromoFalseWholesaleTrue() {
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    notificationService.sendNotificationWithErrorFileGenerated(
        bulkProcess, DESCRIPTION, false, true);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(BULK_PROCESS_CODE, notificationKafkaArgumentCaptor.getValue().getNotificationDetail());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationType()
        .contains(NotificationType.WHOLE_SALE_BULK_UPDATE.getValue()));
  }

  @Test
  public void sendBulkAssignNotificationForSchedules() {
    ReflectionTestUtils.setField(notificationService, "updateScheduleRemovalEnabled", true);
    Mockito.when(applicationProperties.getWebNotifNotificationCreateTopic()).thenReturn(TOPIC_NAME);
    String notificationMessage = "Status telah diubah dan penjadwalan telah dihapus untuk produk "
      + "Nama Produk. Lihat detail : seller.blilbi.com/TEC-15624-00001";
    bulkProcess.setNotes(notificationMessage);
    Map<String, List<BulkProcessData>> map = new HashMap<>();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcessData.setIdentifier(notificationMessage);
    map.put("TEC-15624-00001", Arrays.asList(bulkProcessData));
    notificationService.sendNotificationForSchedulesRemovalForUpsetAndUpdate(map,
      bulkProcess.getBusinessPartnerCode());
    Mockito.verify(applicationProperties, times(1)).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(TOPIC_NAME), Mockito.eq(bulkProcess.getBusinessPartnerCode()),
          notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(notificationMessage,
      notificationKafkaArgumentCaptor.getValue().getNotificationMessage());
  }

  @Test
  public void sendBulkAssignNotificationEnglishForSchedules() {
    ReflectionTestUtils.setField(notificationService, "updateScheduleRemovalEnabled", true);
    Mockito.when(applicationProperties.getWebNotifNotificationCreateTopic()).thenReturn(TOPIC_NAME);
    String notificationMessage =
        "The status has been changed and schedule has been removed for productName. View details : "
            + "seller.blilbi.com/TEC-15624-00001";
    bulkProcess.setNotes(notificationMessage);
    Map<String, List<BulkProcessData>> map = new HashMap<>();
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcessData.setIdentifier(notificationMessage);
    map.put("TEC-15624-00001", Arrays.asList(bulkProcessData));
    notificationService.sendNotificationForSchedulesRemovalForUpsetAndUpdate(map,
      bulkProcess.getBusinessPartnerCode());
    Mockito.verify(applicationProperties, times(1)).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(bulkProcess.getBusinessPartnerCode()), notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(notificationMessage, notificationKafkaArgumentCaptor.getValue().getNotificationMessage());
  }

  @Test
  public void sendNotificationForPromoAndWholesaleStatusPartiallyComplete() {
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    notificationService.sendNotificationWithErrorFileGenerated(
        bulkProcess, DESCRIPTION, true, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(BULK_PROCESS_CODE, notificationKafkaArgumentCaptor.getValue().getNotificationDetail());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationType()
        .contains(NotificationType.BULK_UPDATED_WITH_PROMO.getValue()));
  }

  @Test
  public void sendNotificationForPromoAndWholesaleNonPromo() {
    notificationService.sendNotificationWithErrorFileGenerated(
        bulkProcess, DESCRIPTION, false, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(BULK_PROCESS_CODE, notificationKafkaArgumentCaptor.getValue().getNotificationDetail());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationType()
        .contains(NotificationType.BULK_UPDATED.getValue()));
  }

  @Test
  public void sendNotificationForWorkOrder() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBusinessPartnerCode(MERCHANT_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.ASSEMBLY_REQUEST.getValue());
    notificationService.sendNotificationWithErrorFileGenerated(
      bulkProcess, DESCRIPTION, false, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
      notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(BULK_PROCESS_CODE, notificationKafkaArgumentCaptor.getValue().getNotificationDetail());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationType()
      .contains(NotificationType.BULK_UPLOADED.getValue()));

  }

  @Test
  public void sendNotificationForPromoAndWholesaleWithWholesale() {
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    notificationService.sendNotificationWithErrorFileGenerated(
        bulkProcess, DESCRIPTION, false, true);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(BULK_PROCESS_CODE, notificationKafkaArgumentCaptor.getValue().getNotificationDetail());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationType()
        .contains(NotificationType.WHOLE_SALE_BULK_UPDATE.getValue()));
  }

  @Test
  public void sendNotificationForPromoAndWholesalePromoFalseWholesaleFalse() {
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    notificationService.sendNotificationWithErrorFileGenerated(
        bulkProcess, DESCRIPTION, false, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationType()
        .contains(NotificationType.BULK_UPDATED.getValue()));
  }

  @Test
  public void sendNotificationForPromoAndWholesalePromoFalseWholesaleFalseDescriptionNull() {
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    notificationService.sendNotificationWithErrorFileGenerated(
        bulkProcess, null, false, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationType()
        .contains(NotificationType.BULK_UPDATED.getValue()));
  }

  @Test
  public void sendDownloadNotificationSuccessTrue() throws Exception {
    Mockito.when(fileStorageService.getNotificationType(bulkDownloadRequest.getBulkProcessEntity()))
        .thenReturn(NotificationType.BULK_DOWNLOADED.getValue());
    notificationService.sendDownloadNotification(bulkDownloadRequest, true, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_ID),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
  }

  @Test
  public void sendDownloadNotificationWhenDestinationKeyEmpty() throws Exception {
    bulkDownloadRequest.setMerchantId(null);
    Mockito.when(fileStorageService.getNotificationType(bulkDownloadRequest.getBulkProcessEntity()))
        .thenReturn(NotificationType.BULK_DOWNLOADED.getValue());
    notificationService.sendDownloadNotification(bulkDownloadRequest, true, false);
    Mockito.verify(kafkaProducer, times(0)).send(anyString(), anyString(), Mockito.any(NotificationKafka.class));
  }

  @Test
  public void sendDownloadNotificationSuccessTrueEntityNull() throws Exception {
    bulkDownloadRequest.setBulkProcessEntity(null);
    Mockito.when(fileStorageService.getNotificationType(bulkDownloadRequest.getBulkProcessEntity()))
        .thenReturn(NotificationType.BULK_DOWNLOADED.getValue());
    notificationService.sendDownloadNotification(bulkDownloadRequest, true, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_ID),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
  }

  @Test
  public void sendDownloadNotificationSuccessTrueBulkDownloadEntityPresent() throws Exception {
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.PRODUCT);
    Mockito.when(fileStorageService.getNotificationType(Mockito.any()))
        .thenReturn(NotificationType.BULK_DOWNLOADED.getValue());
    notificationService.sendDownloadNotification(bulkDownloadRequest, true, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_ID),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
  }

  @Test
  public void sendDownloadNotificationSuccessTrueCampaign() throws Exception {
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.CAMPAIGN_PRODUCT);
    Mockito.when(fileStorageService.getNotificationType(bulkDownloadRequest.getBulkProcessEntity()))
        .thenReturn(NotificationType.BULK_DOWNLOADED.getValue());
    notificationService.sendDownloadNotification(bulkDownloadRequest, true, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_ID),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
  }

  @Test
  public void sendDownloadNotificationSuccessTrueDirectDownload() throws Exception {
    bulkDownloadRequest.setDirectDownload(true);
    Mockito.when(fileStorageService.getNotificationType(bulkDownloadRequest.getBulkProcessEntity()))
        .thenReturn(NotificationType.BULK_DOWNLOADED.getValue());
    notificationService.sendDownloadNotification(bulkDownloadRequest, true, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_ID),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
  }

  @Test
  public void sendDownloadNotificationSuccessTrueProductVendor() throws Exception {
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.PRODUCT_VENDOR);
    Mockito.when(fileStorageService.getNotificationType(bulkDownloadRequest.getBulkProcessEntity()))
        .thenReturn(NotificationTypeConstant.VENDOR_BULK_DOWNLOADED);
    notificationService.sendDownloadNotification(bulkDownloadRequest, true, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_ID),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationTypeConstant.VENDOR_BULK_DOWNLOADED,
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
  }

  @Test
  public void sendDownloadNotificationSuccessFalse() throws Exception {
    Mockito.when(fileStorageService.getNotificationType(bulkDownloadRequest.getBulkProcessEntity()))
        .thenReturn(NotificationType.BULK_DOWNLOADED.getValue());
    notificationService.sendDownloadNotification(bulkDownloadRequest, false, false);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_ID),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationMessage()
        .contains(Constant.FAILED_MESSAGE));
  }

  @Test
  public void sendDownloadNotificationSuccessFalsePartialDownloadTrue() throws Exception {
    bulkDownloadRequest.setDirectDownload(true);
    Mockito.when(fileStorageService.getNotificationType(bulkDownloadRequest.getBulkProcessEntity()))
        .thenReturn(NotificationType.BULK_DOWNLOADED.getValue());
    notificationService.sendDownloadNotification(bulkDownloadRequest, true, true);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_ID),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
  }

  @Test
  public void testSendDownloadNotification() {
    bulkDownloadEntity.setStatus(Constant.SUCCESS);
    notificationService.sendDownloadNotification(bulkDownloadEntity);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
  }

  @Test
  public void testSendDownloadNotificationWhenDestinationKeyEmpty() {
    bulkDownloadEntity.setBusinessPartnerCode(null);
    notificationService.sendDownloadNotification(bulkDownloadEntity);
    Mockito.verify(kafkaProducer, times(0)).send(anyString(), anyString(), Mockito.any(NotificationKafka.class));
  }

  @Test
  public void testSendDownloadNotificationFailed() {
    bulkDownloadEntity.setStatus(Constant.FAILED_MESSAGE);
    notificationService.sendDownloadNotification(bulkDownloadEntity);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.BULK_DOWNLOADED.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
  }

  @Test
  public void sendBulkUploadedNotificationStatusFinished() {
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    notificationService.sendBulkUploadedNotification(
        bulkProcess, NotificationType.PROMO_BULK_UPLOAD_ACTIVITY.getValue(), FAILED_LINK);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE), 
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.PROMO_BULK_UPLOAD_ACTIVITY.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
    Assertions.assertFalse(notificationKafkaArgumentCaptor.getValue().getNotificationMessage().contains(FAILED_LINK));
  }

  @Test
  public void sendBulkUploadedNotificationWhenDestinationKeyEmpty() {
    bulkProcess.setBusinessPartnerCode(null);
    notificationService.sendBulkUploadedNotification(bulkProcess,
        NotificationType.PROMO_BULK_UPLOAD_ACTIVITY.getValue(), FAILED_LINK);
    Mockito.verify(kafkaProducer, times(0)).send(anyString(), anyString(), Mockito.any(NotificationKafka.class));
  }

  @Test
  public void sendBulkUploadedNotificationStatusUnFinished() {
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    notificationService.sendBulkUploadedNotification(
        bulkProcess, NotificationType.PROMO_BULK_UPLOAD_ACTIVITY.getValue(), FAILED_LINK);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE), 
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.PROMO_BULK_UPLOAD_ACTIVITY.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
    Assertions.assertTrue(notificationKafkaArgumentCaptor.getValue().getNotificationMessage().contains(FAILED_LINK));
  }

  @Test
  public void sendBulkUploadedNotificationStatusSuccessWithNoFailureLink() {
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    notificationService.sendBulkUploadedNotification(
        bulkProcess, NotificationType.PROMO_BULK_UPLOAD_ACTIVITY.getValue(), StringUtils.EMPTY);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE), 
        notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.PROMO_BULK_UPLOAD_ACTIVITY.getValue(),
        notificationKafkaArgumentCaptor.getValue().getNotificationType());
    Assertions.assertFalse(notificationKafkaArgumentCaptor.getValue().getNotificationMessage().contains(FAILED_LINK));
  }

  @Test
  public void sendQrCodeGeneratedNotification() {
    notificationService.sendGenerateQrCodeNotification(bulkProcess, FILE_PATH);
    Mockito.verify(applicationProperties).getGenerateQrCodePushNotificationTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        generateQrCodeNotificationModelArgumentCaptor.capture());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(),
        generateQrCodeNotificationModelArgumentCaptor.getValue().getMerchantCode());
    Assertions.assertTrue(generateQrCodeNotificationModelArgumentCaptor.getValue().getRedirectionUrl()
        .contains(FILE_PATH));
  }

  @Test
  public void sendQrCodeFailureNotification() {
    notificationService.sendGenerateQrCodeFailedNotification(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getDescription());
    Mockito.verify(applicationProperties).getFailureQrCodePushNotificationTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        generateQrCodeNotificationModelArgumentCaptor.capture());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(),
        generateQrCodeNotificationModelArgumentCaptor.getValue().getMerchantCode());
  }

  @Test
  public void sendQrCodeFailureStoreNotification() {
    bulkProcess.setDescription(Constant.STORE);
    notificationService.sendGenerateQrCodeFailedNotification(bulkProcess.getBusinessPartnerCode(),
        bulkProcess.getDescription());
    Mockito.verify(applicationProperties).getFailureQrCodePushNotificationTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        generateQrCodeNotificationModelArgumentCaptor.capture());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(),
        generateQrCodeNotificationModelArgumentCaptor.getValue().getMerchantCode());
  }

  @Test
  public void sendQrCodeGeneratedNotification_Store() {
    bulkProcess.setDescription(AllowedQRGenerationType.STORE.getValue());
    notificationService.sendGenerateQrCodeNotification(bulkProcess, FILE_PATH);
    Mockito.verify(applicationProperties).getGenerateQrCodePushNotificationTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        generateQrCodeNotificationModelArgumentCaptor.capture());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(),
        generateQrCodeNotificationModelArgumentCaptor.getValue().getMerchantCode());
    Assertions.assertTrue(generateQrCodeNotificationModelArgumentCaptor.getValue().getRedirectionUrl()
        .contains(FILE_PATH));
  }

  @Test
  public void sendQrCodeGeneratedNotification_AllProducts() {
    bulkProcess.setDescription(AllowedQRGenerationType.ALL_PRODUCTS.getValue());
    notificationService.sendGenerateQrCodeNotification(bulkProcess, FILE_PATH);
    Mockito.verify(applicationProperties).getGenerateQrCodePushNotificationTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
        generateQrCodeNotificationModelArgumentCaptor.capture());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(),
        generateQrCodeNotificationModelArgumentCaptor.getValue().getMerchantCode());
    Assertions.assertTrue(generateQrCodeNotificationModelArgumentCaptor.getValue().getRedirectionUrl()
        .contains(FILE_PATH));
  }

  @Test
  public void sendBulkAssignNotificationForScheduleRemoval() {
    notificationService.sendBulkAssignNotification(bulkProcess, 2, 2, VENDOR_CODE);
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
      notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(Constant.ALL_PRODUCTS_SUCCESSFULLY_ASSIGNED,
      notificationKafkaArgumentCaptor.getValue().getNotificationMessage());
  }

  @Test
  public void sendNeedRevisionDeletionNotificationTest() {
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode("process123");
    bulkNeedRevisionDeletion.setProcessType("NEED_REVISION_DELETION_TYPE");
    bulkNeedRevisionDeletion.setBusinessPartnerCode("partnerCode123");
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_READY_TO_PROCESS);
    notificationService.sendNeedRevisionDeletionNotification(bulkNeedRevisionDeletion,"url",1,1);
    Mockito.verify(kafkaProducer,times(2)).send(Mockito.any(), Mockito.any(), notificationKafkaArgumentCaptor.capture());
    Mockito.verify(applicationProperties,times(2)).getWebNotifNotificationCreateTopic();
  }

  @Test
  public void sendNeedRevisionDeletionNotificationForTest() {
    BulkNeedRevisionDeletion bulkNeedRevisionDeletion = new BulkNeedRevisionDeletion();
    bulkNeedRevisionDeletion.setDeletionProcessCode("process123");
    bulkNeedRevisionDeletion.setProcessType("NEED_REVISION_DELETION_TYPE");
    bulkNeedRevisionDeletion.setBusinessPartnerCode("partnerCode123");
    bulkNeedRevisionDeletion.setStatus(BulkNeedRevisionDeletion.STATUS_READY_TO_PROCESS);
    notificationService.sendNeedRevisionDeletionNotification(bulkNeedRevisionDeletion,"url",0,0);
  }

  @Test
  public void sendBulkUploadedNotificationWithoutErrorSheetTest() {
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    notificationService.sendBulkUploadedNotificationWithoutErrorSheet(
      bulkProcess, NotificationType.PROMO_BULK_UPLOAD_ACTIVITY.getValue());
    Mockito.verify(applicationProperties).getWebNotifNotificationCreateTopic();
    Mockito.verify(kafkaProducer).send(Mockito.eq(TOPIC_NAME), Mockito.eq(MERCHANT_CODE),
      notificationKafkaArgumentCaptor.capture());
    Assertions.assertEquals(NotificationType.PROMO_BULK_UPLOAD_ACTIVITY.getValue(),
      notificationKafkaArgumentCaptor.getValue().getNotificationType());
    Assertions.assertFalse(notificationKafkaArgumentCaptor.getValue().getNotificationMessage().contains(FAILED_LINK));
  }
}
