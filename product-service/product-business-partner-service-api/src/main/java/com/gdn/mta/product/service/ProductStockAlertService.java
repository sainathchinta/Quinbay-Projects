package com.gdn.mta.product.service;

import java.util.List;

import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.x.businesspartner.dto.ProfileResponse;


public interface ProductStockAlertService {

  List<String> findGdnSkuStockAlertByBusinessPartnerCode(String businesspartnerCode);

  PbpStockAlert findPbpStockAlertByGdnSkuCode(String gdnSku);
  
  PbpStockAlert findOnePbpStockAlertByGdnSkuCode(String gdnSku);

  /**
   * Return one entry for stock alert by gdnSku
   *
   * @param gdnSku
   * @return
   */
  PbpStockAlert findOnePbpStockAlertByGdnSkuCodeNoMFDCheck(String gdnSku);

  PbpStockAlert createPbpStockAlert(PbpStockAlert pbpStockAlert);

  PbpStockAlert updatePbpStockAlert(PbpStockAlert pbpStockAlert);

  /**
   * set buyable and viewable to false for every item sku that has maximum stock alert attempt
   */
  void autoBatchUpdateItemViewConfig();

  /**
   * send mail and notification for product with stock that is minimum stock alert true or is oos
   * true and oos alert attempt < max oos attempt
   * @throws Exception 
   */

  void sendMailAndNotification() throws Exception;

  void updateDeletedPbpStockAlertByGdnSkuCode(String gdnSku) throws Exception;

  void updateOOS(Level2InventoryOosEvent messageEvent, String itemName, int retry) throws Exception;

  void updateNonOOS(Level2InventoryNonOosEvent messageEvent, String itemName, int retry) throws Exception;
  
  void updateMinimumStock(Level2InventoryMinimumStockAlertEvent messageEvent, String itemName, int retry) throws Exception;

  void updateNonMinimumStock(Level2InventoryMinimumStockAlertEvent messageEvent, String itemName, int retry) throws Exception;

  void archiveGdnSku(String itemSku, boolean archived, int retry) throws Exception;

  /**
   * hide itemSkus which are oos for a certain number of days
   *
   * @param storeId
   * @param requestId
   * @param batchSize
   */
  void hideItemSkuByOOSDate(String storeId, String requestId, int batchSize);

  /**
   * Find merchants by max stock alert attempts
   *
   * @param maxStockAlertAttempt
   * @return
   */
  List<String> findListBusinessPartnerMinimumStock(int maxStockAlertAttempt);

  /**
   * Find stock alerts by merchant
   *
   * @param businessPartnerCode
   * @param maxStockAlertAttempt
   * @return
   * @throws Exception
   */
  List<PbpStockAlert> findPbpStockAlertByBusinessPartnerCode(String businessPartnerCode,
      int maxStockAlertAttempt) throws Exception;

  /**
   * Increment oos_alert_attempt after mailer
   *
   * @param pbpStockAlert
   */
  void updateStockAlertForMailer(PbpStockAlert pbpStockAlert);

  /**
   * Update oos_alert_attempt and send mail, notification
   *
   * @param pbpStockAlertList
   * @param businessPartner
   * @throws Exception
   */
  void sendMailAndNotificationForStockAlert(List<PbpStockAlert> pbpStockAlertList,
      ProfileResponse businessPartner) throws Exception;

  /**
   * delete entries from product stock alert repo by storeId and productId
   * @param storeId
   * @param productId
   */
  void deleteProductStockAlertByStoreIdAndProductId(String storeId, String productId);
}
