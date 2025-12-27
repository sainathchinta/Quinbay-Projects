package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;

public interface CacheItemHelperService {

  Item findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku);

  Item findCacheableByStoreIdAndItemSku(String storeId, String itemSku);

  /**
   * Set cacheable by store Id and ItemSku
   *
   * @param storeId
   * @param itemSku
   * @param item
   */
  void setItemCacheByStoreIdAndItemSku(String storeId, String itemSku, Item item);

  /**
   * Get All Items based on ProductSku
   * @param storeId
   * @param productSku
   * @return
   */
  List<Item> findCacheableByStoreIdAndProductSku(String storeId, String productSku);

  List<Item> findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId,
      String productSku);

  /**
   * Get ItemSkus by PristineId
   * @param storeId
   * @param pristineId
   * @return
   */
  Set<String> findCacheableItemSkusByStoreIdAndPristineIdAndMarkForDeleteFalse(String storeId,
      String pristineId);

  List<Item> findCacheableByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActivated);

  /**
   * @param storeId must not be blank
   * @param itemCode must not be blank
   * @return Item
   */
  Set<String> findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId, String itemCode);

  /**
   * @param storeId must not be blank
   * @param pristineId must not be blank
   * @return Set of string ItemCodes
   */
  Set<String> findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(String storeId,
      String pristineId);

  /**
   * @param storeId must not be blank
   * @param pristineDataItem must not be blank
   * @return Set of string
   */
  Set<String> findItemCodesByPristine(String storeId, PristineDataItem pristineDataItem);

  /**
   * @param storeId must not be blank
   * @param itemCode must not be blank
   * @return Set of string ActivePromoBundling
   */
  Set<String> findCacheableActivePromoBundlingsByStoreIdAndItemCode(String storeId, String itemCode);

  /**
   *
   * @param storeId
   * @param itemCode
   * @param pickupPointCode
   * @return
   */
  Set<String> findCacheableActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(String storeId,
      String itemCode, String pickupPointCode);

  /**
   * @param storeId must not be blank
   * @param pristineDataItem must not be null
   * @return Set of string ActivePromoBundling
   */
  Set<String> findCacheableActivePromoBundlingsByStoreIdAndPristine(String storeId, PristineDataItem pristineDataItem);

  /**
   *
   * @param storeId
   * @param pristineDataItem
   * @param pickupPointCode
   * @return
   */
  Set<String> findCacheableActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(String storeId,
      PristineDataItem pristineDataItem, String pickupPointCode);

  /**
   * multi get items present in cache
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  Map<String, Item> getCacheableItemsByItemSkus(String storeId, Set<String> itemSkus);

  /** Get Cacheable Items by ProductSkus by multiGet  operation
   * @param storeId
   * @param productSkus
   * @return
   */
  List<Item> getCacheableItemsByProductSkus(String storeId, Set<String> productSkus);

  /**
   * Get Cacheable Items by ProductSkus by multiGet  operation for suspension list
   *
   * @param storeId
   * @param productSkus
   * @return
   */
  List<Item> getCacheableItemsByProductSkusForSuspensionList(String storeId, Set<String> productSkus);
}
