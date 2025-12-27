package com.gdn.mta.product.service;

import java.util.List;

/**
 * @author anand
 * @since Sep 2019
 */
public interface CreateProductSyncService {

  /**
   * Create duplicate / clone product using item sku (Same as flow 2 creation)
   *
   * @param storeId     store unique identifier (multi-tenant)
   * @param username    authenticated user name
   * @param partnerCode business partner code
   * @param pickupPoint business partner pickup point
   * @param itemSkus     item skus which are being cloned
   */
  void createSyncProduct(String storeId, String username, String partnerCode, String pickupPoint, List<String> itemSkus) throws Exception;
}
