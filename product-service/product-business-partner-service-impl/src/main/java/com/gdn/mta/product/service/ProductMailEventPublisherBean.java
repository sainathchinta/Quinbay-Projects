package com.gdn.mta.product.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.partners.pbp.commons.constants.DomainKeyConstants;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailDomainEvent;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductMailEventPublisherBean implements ProductMailEventPublisher {

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Override
  public ProductMailDomainEvent publishProductMailDomainEventSendForCorrection(
      ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_SEND_FOR_CORRECTION + "with "
        + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_SEND_FOR_CORRECTION, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishProductMailDomainEventActive(
      ProductMailDomainEvent productMailDomainEvent) {
    log.debug(
        "Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_APPROVED_PRODUCT + "with " + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_APPROVED_PRODUCT, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishPostLiveReviewApprovedProductMailDomainEvent(
      ProductMailDomainEvent productMailDomainEvent) {
    log.info("Publishing event: {}, message: {}", DomainEventName.POST_LIVE_REVIEW_PRODUCT_APPROVED,
        productMailDomainEvent);
    kafkaProducer.send(DomainEventName.POST_LIVE_REVIEW_PRODUCT_APPROVED, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishPostLiveReviewRejectedProductMailDomainEvent(
      ProductMailDomainEvent productMailDomainEvent) {
    log.info("Publishing event: {}, message: {}", DomainEventName.POST_LIVE_REVIEW_PRODUCT_REJECTED,
        productMailDomainEvent);
    kafkaProducer.send(DomainEventName.POST_LIVE_REVIEW_PRODUCT_REJECTED, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishProductMailDomainEventRejected(
      ProductMailDomainEvent productMailDomainEvent) {
    log.debug(
        "Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_REJECTED_PRODUCT + "with " + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_REJECTED_PRODUCT, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishProductMailDomainEventCategoryChange(
      ProductMailDomainEvent productMailDomainEvent) {
    log.debug(
        "Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_CATEGORY_CHANGE + "with " + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_CATEGORY_CHANGE, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishProductMailDomainEventSendForCorrectionEn(
      ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_SEND_FOR_CORRECTION_EN + "with "
        + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_SEND_FOR_CORRECTION_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishProductMailDomainEventActiveEn(
      ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_APPROVED_PRODUCT_EN + "with "
        + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_APPROVED_PRODUCT_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishPostLiveReviewApprovedProductMailDomainEventEn(
      ProductMailDomainEvent productMailDomainEvent) {
    log.info("Publishing event: {}, message: {}", DomainEventName.POST_LIVE_REVIEW_PRODUCT_APPROVED_EN,
        productMailDomainEvent);
    kafkaProducer.send(DomainEventName.POST_LIVE_REVIEW_PRODUCT_APPROVED_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishPostLiveReviewRejectedProductMailDomainEventEn(
      ProductMailDomainEvent productMailDomainEvent) {
    log.info("Publishing event: {}, message: {}", DomainEventName.POST_LIVE_REVIEW_PRODUCT_REJECTED_EN,
        productMailDomainEvent);
    kafkaProducer.send(DomainEventName.POST_LIVE_REVIEW_PRODUCT_REJECTED_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishItemSkuArchivedEventEn(
      ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_AUTO_ARCHIVE_ITEM_SKU_EN + "with "
        + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_AUTO_ARCHIVE_ITEM_SKU_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishItemSkuArchivedEvent(ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_AUTO_ARCHIVE_ITEM_SKU + "with "
        + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_AUTO_ARCHIVE_ITEM_SKU, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishOOSItemSkuEvent(ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_OOS_ITEM_SKU + "with" + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_OOS_ITEM_SKU, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishOOSItemSkuEventEn(ProductMailDomainEvent productMailDomainEvent) {
    log.debug(
        "Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_OOS_ITEM_SKU_EN + "with" + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_OOS_ITEM_SKU_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishMinStockItemSkuEvent(ProductMailDomainEvent productMailDomainEvent) {
    log.debug(
        "Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_MIN_STOCK_ITEM_SKU + "with" + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_MIN_STOCK_ITEM_SKU, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishMinStockItemSkuEventEn(ProductMailDomainEvent productMailDomainEvent) {
    log.debug(
        "Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_MIN_STOCK_ITEM_SKU_EN + "with" + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_MIN_STOCK_ITEM_SKU_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishItemReActivationMailEventEn(ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event: {} " + DomainEventName.PRODUCT_MAIL_EVENT_RE_ACTIVATED_PRODUCTS_EN + "with : {} "
        + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_RE_ACTIVATED_PRODUCTS_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishItemReActivationMailEvent(ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event: {} " + DomainEventName.PRODUCT_MAIL_EVENT_RE_ACTIVATED_PRODUCTS + "with : {} "
        + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_RE_ACTIVATED_PRODUCTS, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishItemSuspensionMailEventEn(ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event: {} " + DomainEventName.PRODUCT_MAIL_EVENT_SUSPENDED_PRODUCTS_EN + "with : {} "
        + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_SUSPENDED_PRODUCTS_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishItemSuspensionMailEvent(ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event: {} " + DomainEventName.PRODUCT_MAIL_EVENT_SUSPENDED_PRODUCTS + "with : {} "
        + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_SUSPENDED_PRODUCTS, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishProductMailDomainEventRejectedEn(
      ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_REJECTED_PRODUCT_EN
        +"with " + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_REJECTED_PRODUCT_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

  @Override
  public ProductMailDomainEvent publishProductMailDomainEventCategoryChangeEn(
      ProductMailDomainEvent productMailDomainEvent) {
    log.debug("Publishing event:" + DomainEventName.PRODUCT_MAIL_EVENT_CATEGORY_CHANGE_EN
        +"with " + productMailDomainEvent);
    kafkaProducer.send(DomainEventName.PRODUCT_MAIL_EVENT_CATEGORY_CHANGE_EN, productMailDomainEvent.getMerchantCode(),
        productMailDomainEvent);
    return productMailDomainEvent;
  }

}