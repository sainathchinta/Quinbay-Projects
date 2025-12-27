package com.gdn.mta.product.service;

public interface ProductStockAlertWrapper {

  /**
   * Send mail and notification for OOS and mint stock alerts
   *
   * @param requestId
   * @param username
   */
  void sendMailAndNotification(String requestId, String username);
}
