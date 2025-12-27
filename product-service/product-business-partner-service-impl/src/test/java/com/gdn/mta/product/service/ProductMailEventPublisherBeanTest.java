package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.List;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailDomainEvent;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class ProductMailEventPublisherBeanTest {
  private static final String PRODUCT_SKU = "producSku";
  private static final String DEFAULT_PRODUCT_NAME = "product name";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BP-00001";
  private static final String NOTES = "notes";
  private static final List<String> ITEM_SKU_LIST = new ArrayList<>();
  private static final List<String> ITEM_SKU_NAME = new ArrayList<>();
  private static final List<List<String>> DEFAULT_PRODUCT_DATAS = new ArrayList<>();

  @InjectMocks
  ProductMailEventPublisherBean productMailEventPublisher;

  private ProductMailDomainEvent productMailDomainEvent;

  @Mock
  private KafkaPublisher kafkaProducer;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ITEM_SKU_LIST.add(PRODUCT_SKU);
    ITEM_SKU_NAME.add(DEFAULT_PRODUCT_NAME);
    DEFAULT_PRODUCT_DATAS.add(ITEM_SKU_LIST);
    DEFAULT_PRODUCT_DATAS.add(ITEM_SKU_NAME);
  }

  @Test
  public void publishSendForCorrectionTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.SENT_FOR_CORRECTION.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishProductMailDomainEventSendForCorrection(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_SEND_FOR_CORRECTION), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(productMailDomainEvent.getNotificationType(), result.getNotificationType());
    Assertions.assertEquals(result.getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void publishSendForCorrectionEnTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.SENT_FOR_CORRECTION.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishProductMailDomainEventSendForCorrectionEn(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_SEND_FOR_CORRECTION_EN), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(productMailDomainEvent.getNotificationType(), result.getNotificationType());
    Assertions.assertEquals(result.getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void publishActiveTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.APPROVED.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishProductMailDomainEventActive(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_APPROVED_PRODUCT), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(productMailDomainEvent.getNotificationType(), result.getNotificationType());
    Assertions.assertEquals(result.getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void publishActiveEnTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.APPROVED.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishProductMailDomainEventActiveEn(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_APPROVED_PRODUCT_EN), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(productMailDomainEvent.getNotificationType(), result.getNotificationType());
    Assertions.assertEquals(result.getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void publishRejectedTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.REJECTED.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishProductMailDomainEventRejected(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_REJECTED_PRODUCT), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(productMailDomainEvent.getNotificationType(), result.getNotificationType());
    Assertions.assertEquals(result.getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void publishRejectedEnTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.REJECTED.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishProductMailDomainEventRejectedEn(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_REJECTED_PRODUCT_EN), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(productMailDomainEvent.getNotificationType(), result.getNotificationType());
    Assertions.assertEquals(result.getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void publishCategoryChangeTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.CATEGORY_CHANGE.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishProductMailDomainEventCategoryChange(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_CATEGORY_CHANGE), Mockito.any(), Mockito.any());
    Assertions.assertEquals(productMailDomainEvent.getNotificationType(), result.getNotificationType());
    Assertions.assertEquals(result.getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void publishCategoryChangeEnTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.CATEGORY_CHANGE.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishProductMailDomainEventCategoryChangeEn(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_CATEGORY_CHANGE_EN), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(productMailDomainEvent.getNotificationType(), result.getNotificationType());
    Assertions.assertEquals(result.getProductSku(), PRODUCT_SKU);
  }

  @Test
  public void publishOOSItemSkuEventTest() {
    productMailDomainEvent =
        ProductMailDomainEvent.builder().productDatas(DEFAULT_PRODUCT_DATAS).merchantCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .build();
    ProductMailDomainEvent result = this.productMailEventPublisher.publishOOSItemSkuEvent(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_OOS_ITEM_SKU), Mockito.any(), Mockito.any());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.getMerchantCode());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductDatas().get(0).get(0));
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, result.getProductDatas().get(1).get(0));
  }


  @Test
  public void publishOOSItemSkuEventEnTest() {
    productMailDomainEvent =
        ProductMailDomainEvent.builder().productDatas(DEFAULT_PRODUCT_DATAS).merchantCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .build();
    ProductMailDomainEvent result = this.productMailEventPublisher.publishOOSItemSkuEventEn(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_OOS_ITEM_SKU_EN), Mockito.any(), Mockito.any());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.getMerchantCode());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductDatas().get(0).get(0));
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, result.getProductDatas().get(1).get(0));
  }

  @Test
  public void publishMinStockItemSkuEventTest() {
    productMailDomainEvent =
        ProductMailDomainEvent.builder().productDatas(DEFAULT_PRODUCT_DATAS).merchantCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .build();
    ProductMailDomainEvent result = this.productMailEventPublisher.publishMinStockItemSkuEvent(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_MIN_STOCK_ITEM_SKU), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.getMerchantCode());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductDatas().get(0).get(0));
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, result.getProductDatas().get(1).get(0));
  }


  @Test
  public void publishMinStockItemSkuEventEnTest() {
    productMailDomainEvent =
        ProductMailDomainEvent.builder().productDatas(DEFAULT_PRODUCT_DATAS).merchantCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishMinStockItemSkuEventEn(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_MIN_STOCK_ITEM_SKU_EN), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.getMerchantCode());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductDatas().get(0).get(0));
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, result.getProductDatas().get(1).get(0));
  }

  @Test
  public void publishItemReActivationMailEventEnTest() {
    productMailDomainEvent =
        ProductMailDomainEvent.builder().productDatas(DEFAULT_PRODUCT_DATAS).merchantCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .notes(NOTES).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishItemReActivationMailEventEn(productMailDomainEvent);
    System.out.println(result);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_RE_ACTIVATED_PRODUCTS_EN), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.getMerchantCode());
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, result.getProductDatas().get(1).get(0));
    Assertions.assertEquals(NOTES, result.getNotes());
  }

  @Test
  public void publishItemReActivationMailEventTest() {
    productMailDomainEvent =
        ProductMailDomainEvent.builder().productDatas(DEFAULT_PRODUCT_DATAS).merchantCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .notes(NOTES).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishItemReActivationMailEvent(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_RE_ACTIVATED_PRODUCTS), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.getMerchantCode());
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, result.getProductDatas().get(1).get(0));
    Assertions.assertEquals(NOTES, result.getNotes());
  }

  @Test
  public void publishItemSuspensionMailEventEnTest() {
    productMailDomainEvent =
        ProductMailDomainEvent.builder().productDatas(DEFAULT_PRODUCT_DATAS).merchantCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .notes(NOTES).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishItemSuspensionMailEventEn(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_SUSPENDED_PRODUCTS_EN), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.getMerchantCode());
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, result.getProductDatas().get(1).get(0));
    Assertions.assertEquals(NOTES, result.getNotes());
  }

  @Test
  public void publishItemSuspensionMailEventTest() {
    productMailDomainEvent =
        ProductMailDomainEvent.builder().productDatas(DEFAULT_PRODUCT_DATAS).merchantCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .notes(NOTES).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishItemSuspensionMailEvent(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_MAIL_EVENT_SUSPENDED_PRODUCTS), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.getMerchantCode());
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, result.getProductDatas().get(1).get(0));
    Assertions.assertEquals(NOTES, result.getNotes());
  }

  @Test
  public void publishPostLiveReviewProductMailDomainEventEnTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishPostLiveReviewApprovedProductMailDomainEventEn(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.POST_LIVE_REVIEW_PRODUCT_APPROVED_EN), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED.getNotificationType(),
        result.getNotificationType());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductSku());
  }

  @Test
  public void publishPostLiveReviewProductMailDomainEventTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishPostLiveReviewApprovedProductMailDomainEvent(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.POST_LIVE_REVIEW_PRODUCT_APPROVED), Mockito.any(), Mockito.any());
    Assertions.assertEquals(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED.getNotificationType(),
        result.getNotificationType());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductSku());
  }

  @Test
  public void publishPostLiveReviewRejectProductMailDomainEventEnTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishPostLiveReviewRejectedProductMailDomainEventEn(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.POST_LIVE_REVIEW_PRODUCT_REJECTED_EN), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED.getNotificationType(),
        result.getNotificationType());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductSku());
  }

  @Test
  public void publishPostLiveReviewRejectProductMailDomainEventTest() {
    productMailDomainEvent = ProductMailDomainEvent.builder().productSku(PRODUCT_SKU)
        .notificationType(ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED.getNotificationType()).build();
    ProductMailDomainEvent result =
        this.productMailEventPublisher.publishPostLiveReviewRejectedProductMailDomainEvent(productMailDomainEvent);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.POST_LIVE_REVIEW_PRODUCT_REJECTED), Mockito.any(), Mockito.any());
    Assertions.assertEquals(ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED.getNotificationType(),
        result.getNotificationType());
    Assertions.assertEquals(PRODUCT_SKU, result.getProductSku());
  }
}