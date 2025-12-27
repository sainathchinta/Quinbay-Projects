package com.gdn.partners.pbp.service.notification;

import com.gdn.partners.pbp.entity.mailEvent.CategoryChangeMailEvent;

public interface ProductNotificationService {

  void sendProductActiveNotification(String businessPartnerCode, String gdnSku) throws Exception;

  void sendProductStockMinimalNotification(String businessPartnerCode, Integer productCount)
      throws Exception;

  void sendProductStockOosNotification(String businessPartnerCode, Integer productCount)
      throws Exception;

  /**
   * send Product Reject Notification
   * @param businessPartnerCode
   * @param productName
   */
  void sendProductRejectNotification(String businessPartnerCode, String productName)
      throws Exception;

  /**
   * Send return for correction mail
   * @param businessPartnerCode
   * @param productName
   * @param gdnSku
   * @param notes
   */
  void sendProductReturnForCorrectionNotification(String businessPartnerCode, String productName,
      String gdnSku, String notes) throws Exception;

  /**
   * Set activation notification with category change
   *
   * @param businessPartnerCode
   * @param categoryChangeMailEvent
   * @param productSku
   * @param isBundleProduct
   */
  void sendProductActiveNotification(String businessPartnerCode, CategoryChangeMailEvent categoryChangeMailEvent, String productSku, boolean isBundleProduct) throws Exception;

  /**
   * @param businessPartnerCode
   * @param gdnProductSku
   * @param productName
   * @param isBundleProduct
   */
  void sendProductActiveNotification(String businessPartnerCode, String gdnProductSku, String productName,  boolean isBundleProduct)
      throws Exception;

  /**
   * Api to trigger the notification for QR codes download.
   *
   * @param businessPartnerCode
   * @param filepath
   */
  void sendProductQRCodeDownloadNotification(String businessPartnerCode, String filepath) throws Exception;

  /**
   * send the notification for processes finished copying products.
   * @param businessPartnerCode
   * @param message
   */
  void sendProductSyncNotification(String businessPartnerCode, String message) throws Exception;

  /**
   * send the notification for created products with restricted keyword
   *
   * @param businessPartnerCode
   * @param productName
   * @throws Exception
   */
  void sendNotificationForProductWithRestrictedKeyword(String businessPartnerCode, String productName,
      boolean isInternational) throws Exception;

  /**
   * send the notification for product status change
   *
   * @param businessPartnerCode
   * @param productName
   * @param status
   * @throws Exception
   */
  void sendNotificationForProductStatus(String businessPartnerCode, String productName, String status) throws Exception;

}
