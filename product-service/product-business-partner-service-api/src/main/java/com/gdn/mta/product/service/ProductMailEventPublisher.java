package com.gdn.mta.product.service;

import com.gdn.partners.pbp.entity.mailEvent.ProductMailDomainEvent;

public interface ProductMailEventPublisher {
  /**
   * Publish Product Mail Event
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishProductMailDomainEventSendForCorrection(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for active
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishProductMailDomainEventActive(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishPostLiveReviewApprovedProductMailDomainEvent(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * publish post live rejection mail
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishPostLiveReviewRejectedProductMailDomainEvent(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for rejected
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishProductMailDomainEventRejected(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for category change
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishProductMailDomainEventCategoryChange(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * Publish Product Mail Event in english
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishProductMailDomainEventSendForCorrectionEn(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for active in english
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishProductMailDomainEventActiveEn(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for rejected in english
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishProductMailDomainEventRejectedEn(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for category change in english
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishProductMailDomainEventCategoryChangeEn(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishPostLiveReviewApprovedProductMailDomainEventEn(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * Publish post live review rejection english mail
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishPostLiveReviewRejectedProductMailDomainEventEn(
      ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for auto archiving itemSku for OOS in english.
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishItemSkuArchivedEventEn(ProductMailDomainEvent productMailDomainEvent);


  /**
   * send mail event for auto archiving itemSku for OOS.
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishItemSkuArchivedEvent(ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for products which are OOS
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishOOSItemSkuEvent(ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for products which are OOS in English
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishOOSItemSkuEventEn(ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for products which are minimum stock
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishMinStockItemSkuEvent(ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for products which are minimum stock in English
   *
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishMinStockItemSkuEventEn(ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for itemSku re-activation in English
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishItemReActivationMailEventEn(ProductMailDomainEvent productMailDomainEvent);

  /**
   * send mail event for itemSku re-activation
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishItemReActivationMailEvent(ProductMailDomainEvent productMailDomainEvent);

  /**
   * send email event for itemSku suspension in English
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishItemSuspensionMailEventEn(ProductMailDomainEvent productMailDomainEvent);

  /**
   * send email event for itemSku suspension
   * @param productMailDomainEvent
   * @return
   */
  ProductMailDomainEvent publishItemSuspensionMailEvent(ProductMailDomainEvent productMailDomainEvent);

}
