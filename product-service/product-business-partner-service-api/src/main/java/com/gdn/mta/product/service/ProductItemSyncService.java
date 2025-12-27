package com.gdn.mta.product.service;

import java.util.List;
import java.util.Map;

import com.gdn.mta.product.entity.ProductItemSyncProcessSummary;
import com.gdn.mta.product.entity.ProductItemSyncStatus;

/**
 * @author anand
 * @since Sep 2019
 */
public interface ProductItemSyncService {

  ProductItemSyncStatus save(ProductItemSyncStatus syncStatus);

  ProductItemSyncStatus findByItemSkuAndBusinessPartnerCode(String storeId, String itemSku, String partnerCode);

  /**
   * Find all products which are already copied from linked partner code to business partner account
   *
   * @param storeId             store identifier
   * @param businessPartnerCode business partner code (target)
   * @param linkedPartnerCode   linked partner code (source)
   *
   * @return list of already copied products to linked partner account
   */
  List<String> findItemsAlreadyInSyncProcess(String storeId, String businessPartnerCode, String linkedPartnerCode);

  /**
   * Find count of items corresponding to all the ProductSyncStatus by process Id
   *
   * @param storeId             store identifier
   * @param processId           process Id
   *
   * @return list of in process products to linked partner account
   */
   List<ProductItemSyncProcessSummary> productCopyStatusForProcessID(String storeId, String processId);

  /**
   * Create duplicate item (L3 & L4) using existing item, required details will be copied from existing item
   *  @param storeId             store identifier
   * @param username            username
   * @param businessPartnerCode business partner code
   * @param pickupPointCode     pickup point code
   * @param isRetryAttempt      is retry attempt
   * @param processId           process Id
   * @param itemSkus            items to be copied
   * @param linkedPartnerCode   source business partner code for product copying
   * @return
   */
  Integer copy(String storeId, String username, String businessPartnerCode, String pickupPointCode, boolean isRetryAttempt,
    String processId, Map<String, List<String>> itemSkus, String linkedPartnerCode);

  /**
   * Find all products which are already copied from linked partner code to business partner account
   *
   * @param storeId             store identifier
   * @param businessPartnerCode business partner code (target)
   * @param linkedPartnerCode   linked partner code (source)
   * @param itemSKUs            item skus filter
   *
   * @return list of already copied products to linked partner account
   */
  List<ProductItemSyncStatus> findSyncStatusByItemSkuAndLinkedPartner(String storeId, String businessPartnerCode,
    String linkedPartnerCode, List<String> itemSKUs);

  /**
   * update IN_PROGRESS process status to FAIL updates before the syncRetryDuration
   *
   * @param storeId             store identifier
   * @param syncRetryDuration   sync retry duration
   *
   */
  void updateProductItemSyncStatus(String storeId, long syncRetryDuration);

}
