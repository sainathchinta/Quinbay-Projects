package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.vo.ItemAndItemPickupPointVo;

public interface ItemCacheableService {

  /**
   * Get item based on itemSku and mark for delete false
   *
   * @param storeId
   * @param itemSku
   * @param combineOthersBundlings
   * @param off2On
   * @param fbbActivated
   * @return item
   */
  Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, boolean combineOthersBundlings,
    boolean off2On, boolean fbbActivated);

  /**
   * Get item based on itemSku and mark for delete false
   *
   * @param storeId
   * @param itemSku
   * @param fullFetch
   * @param combineOthersBundlings
   * @param instantPickup
   * @param pickupPointCode
   * @param off2On
   * @param fbbActivated
   * @return item
   */
  Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, boolean fullFetch,
    boolean combineOthersBundlings, boolean instantPickup, String pickupPointCode, boolean off2On,
    boolean fbbActivated);

  /**
   * Get item based on itemSku and mark for delete false
   *
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return item
   */
  ItemAndItemPickupPointVo findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String itemSku, String pickupPointCode);

  /**
   *
   * @param storeId
   * @param productSkus
   * @param itemSku
   * @param page
   * @param pageSize
   * @return
   */
  ItemAndItemPickupPointVo findItemAndItemPickPointByproductSkus(String storeId,
      List<String> productSkus, List<String> itemSku, boolean showDeleted, int page, int pageSize);

  /**
   * Get item based on itemSku and mark for delete false
   *
   * @param storeId
   * @param itemSku
   * @param fullFetch
   * @param combineOthersBundlings
   * @param instantPickup
   * @param pickupPointCode
   * @param off2On
   * @return item
   */
  Item findItemByStoreIdAndItemSku(String storeId, String itemSku, boolean fullFetch,
      boolean combineOthersBundlings, boolean instantPickup, String pickupPointCode, boolean off2On);

  Map<String, Item> findItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(String storeId,
      Set<String> itemSkus);

  /**
   * Get All Items based on productSku and mark for delete false
   *
   * @param storeId
   * @param productSku
   * @param combineOthersBundlings
   * @param off2On
   * @param fbbActivated
   * @return list of items
   */
  List<Item> findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku,
    boolean combineOthersBundlings, boolean off2On, boolean fbbActivated);

  /**
   * Get All Items based on productSku and show deleted false
   *
   * @param storeId
   * @param productSku
   * @param combineOthersBundlings
   * @param off2On
   * @param showDeleted
   * @return
   */
  List<Item> findItemsByStoreIdAndProductSkuAndShowDeletedFlag(String storeId, String productSku,
      boolean combineOthersBundlings, boolean off2On, boolean showDeleted, Map<String, ItemPickupPoint> itemPickupPointMap);

  /**
   * Get All Items based on productSku
   *
   * @param storeId
   * @param productSku
   * @param combineOthersBundlings
   * @param off2On
   * @param isMigrateAndSyncProduct
   * @return
   */
  List<Item> findItemsByStoreIdAndProductSku(String storeId, String productSku, boolean combineOthersBundlings,
      boolean off2On, boolean isMigrateAndSyncProduct);

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param pristineId
   * @param off2On
   * @return
   */
  List<Item> findItemsByPristineId(String storeId, String username, String requestId, String pristineId,
      boolean off2On);

  /**
   * Get items by pristine id
   * @param storeId
   * @param pristineId
   * @param off2On
   * @return
   */
  List<Item> getItemsByPristineId(String storeId, String pristineId, boolean off2On);

  List<Item> findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActivated);

  /**
   * Find all items by item code from cache first if not then from DB
   *
   * @param storeId must not be blank
   * @param itemCode must not be blank
   * @return list of items
   */
  List<Item> findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId, String itemCode);

  /**
   * @param storeId must not be blank
   * @param pristineDataItem must not be null
   * @return set of string itemCodes
   */
  Set<String> findItemCodesByPristine(String storeId, PristineDataItem pristineDataItem);

  /**
   * find
   * @param storeId must not be blank
   * @param pristineId must not be blank
   * @return Set of string itemCodes
   */
  Set<String> findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(String storeId,
      String pristineId);

  Set<String> findCacheableActivePromoBundlingsByStoreIdAndItemCode(String storeId,
      String itemCode);

  Set<String> findCacheableActivePromoBundlingsByStoreIdAndPristine(String storeId,
      PristineDataItem pristineDataItem);

  /**
   * @param storeId must not be blank
   * @param item must not be null
   */
  void setActivePromoBundlingsByPristineOrItemCode(String storeId, Item item);

  /**
   * Get Cacheable Items By productSkus by multiGet operation
   * @param storeId
   * @param productSkus
   * @return
   */
  List<Item> getCacheableItemsByProductSkus(String storeId, Set<String> productSkus);

  /**
   * Get All itemSkus by itemCode
   * @param storeId
   * @param itemCode
   * @return
   */
  Set<String> findAllItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId,
      String itemCode);

  /**
   * @param storeId     must not be null
   * @param productSkus must not be empty
   * @param l5IdList
   * @param cncActivated       must not be null
   * @param page        must not be null
   * @param pageSize    must not be null
   * @return
   */
  ItemAndItemPickupPointVo findItemAndItemPickPointByproductSkusAndCncActive(String storeId, List<String> productSkus,
    List<String> l5IdList, Boolean cncActivated, Integer page, Integer pageSize);

}
