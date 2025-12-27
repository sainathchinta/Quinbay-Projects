package com.gdn.x.product.service.api;

public interface CacheEvictItemService {

  void evictFindItemByItemSku(String storeId, String itemSku);

  void evictFindItemByMerchantSku(String storeId, String merchantSku);

  void evictFindItemByProductSku(String storeId, String productSku);

  /**
   * @param storeId
   * @param pristineId
   */
  void evictFindItemSkusByPristineId(String storeId, String pristineId);

  /**
   * @param storeId must not be blank
   * @param itemCode must not be blank
   */
  void evictFindAllActivePromoBundlingsByItemCode(String storeId, String itemCode);

  /**
   * Evict cache findItemPickupPointByItemSkuAndPickupPointCode
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   */
  void evictFindItemPickupPointByItemSkuAndPickupPointCode(String storeId, String itemSku,
    String pickupPointCode);

  /**
   * @param storeId must not be blank
   * @param pristineId must not be blank
   */
  void evictFindAllActivePromoBundlingsByPristineId(String storeId, String pristineId);

  /**
   *
   * @param storeId
   * @param itemCode
   * @param pickupPointCode
   */
  void evictFindAllActivePromoBundlingsByItemCode(String storeId, String itemCode, String pickupPointCode);

  /**
   *
   * @param storeId
   * @param pristineId
   * @param pickupPointCode
   */
  void evictFindAllActivePromoBundlingsByPristineId(String storeId, String pristineId, String pickupPointCode);

  void evictUniqueIdTypeCheck(String storeId, String offlineItemId);

  /**
   * @param storeId must not be blank
   * @param itemCode must not be blank
   */
  void evictFindAllByStoreIdAndItemCode(String storeId, String itemCode);


  /**
   * Evict cache findItemCodesByPristine
   * @param storeId
   * @param pristineId
   */
  void evictFindItemCodesByPristine(String storeId, String pristineId);

  /**
   * Evict cache findByItemSkuAndDelivery
   * @param storeId
   * @param itemSku
   */
  void evictFindL5ByItemSku(String storeId, String itemSku);
}
