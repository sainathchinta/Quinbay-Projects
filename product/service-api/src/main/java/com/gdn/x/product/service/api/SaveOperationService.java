package com.gdn.x.product.service.api;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;

public interface SaveOperationService {

  Item addActivePromoBundling(String storeId, String itemSku, String promoBundlingType);

  /**
   * Update promo bundling type
   *
   * @param storeId
   * @param itemSku
   * @param promoBundlingType
   * @return
   */
  Item updateActivePromoBundling(String storeId, String itemSku, String promoBundlingType);

  /**
   * @param item
   * @return
   */
  Item insertItem(Item item);

  /**
   * @param items
   * @return
   */
  List<Item> insertItems(List<Item> items);

  Item removeActivePromoBundling(String storeId, String itemSku, String promoBundlingType);

  /**
   * @param item
   * @param itemPickupPointList
   * @param itemPickupPointDataChangeEventModelList
   * @return
   */
  Item saveItem(Item item, List<ItemPickupPoint> itemPickupPointList,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList);

  /**
   * Save item without updating solr
   *
   * @param item
   * @param savedItemPickupPoints
   * @param productRejected              true if the product is rejected, false otherwise
   * @param eventToBlackListedSellersMap
   * @return
   */
  Item saveItemWithoutUpdatingSolr(Item item, List<ItemPickupPoint> savedItemPickupPoints,
    boolean productRejected, String source, Map<String, Set<String>> eventToBlackListedSellersMap);

  /**
   * @param listOfItems must not be null
   * @param itemPickupPointList
   * @return never null
   */
  List<Item> saveItems(List<Item> listOfItems, List<ItemPickupPoint> itemPickupPointList);


  List<Item> saveNewlyAddedItems(List<Item> items);

  /**
   * Save items without updating solr
   *
   * @param listOfItems
   * @return
   */
  List<Item> saveItemsWithoutUpdatingSolr(List<Item> listOfItems);


  /**
   * Save items and clear cache without updating solr
   *
   * @param listOfItems
   * @param itemPickupPointList
   * @return
   */
  List<Item> saveItemsAndClearCacheWithoutUpdatingSolr(List<Item> listOfItems,
      List<ItemPickupPoint> itemPickupPointList, String source);

  /**
   * Save items and clear cache without updating solr without publishing
   *
   * @param listOfItems
   * @return
   */
  List<Item> saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(List<Item> listOfItems);

  /**
   *
   * @param items
   * @param itemPickupPointList
   * @return
   */
  List<Item> saveItemsWithDeferredReindexing(List<Item> items, List<ItemPickupPoint> itemPickupPointList);


  /**
   * Deffered reindex offline item
   *  @param storeId
   * @param items
   * @param updatedOfflineItem
   * @param reindexType
   * @param updateL3PickupPointCodes
   */
  void deferredReindexItems(String storeId, List<Item> items, boolean updatedOfflineItem,
    ReindexType reindexType, boolean updateL3PickupPointCodes);

  /**
   * @param product
   * @return
   */
  Product saveProduct(Product product);

  /**
   * @param product
   * @param productChangeEventTypes
   * @param productPublishSourceDeletePickupPoint
   * @param eventToBlackListedSellersMap
   * @return
   */
  Product saveProductWithoutUpdatingSolr(Product product, List<String> productChangeEventTypes,
    String productPublishSourceDeletePickupPoint,
    Map<String, Set<String>> eventToBlackListedSellersMap);

  /**
   * @param product
   * @param itemVos
   * @return
   */
  ProductAndItemsVO saveProductAndItemsAndPickupPoint(Product product, List<ItemVo> itemVos);

  /**
   * @param productAndItems
   * @param productChangeTypes
   * @return
   */
  ProductAndItemsVO saveProductAndItems(ProductAndItemsVO productAndItems, List<String> productChangeTypes);

  /**
   *
   * @param productAndItems
   * @return
   */
  ProductAndItemsVO saveProductAndItemsWithoutPublishingEvent(ProductAndItemsVO productAndItems);

  /**
   * Save product and items without publishing event and reindexing
   *
   * @param productAndItems
   * @return
   */
  ProductAndItemsVO saveProductAndItemsWithoutPublishingEventAndWithoutReindexing(ProductAndItemsVO productAndItems);

  /**
   * @param productAndItems
   * @return
   */
  ProductAndItemsVO saveProductAndItemsWithoutPublishingItemChange(ProductAndItemsVO productAndItems);

  /**
   * Save saveProductAndItems on sync and unsync without updating it in solr
   *
   * @param productAndItems
   * @return
   */
  ProductAndItemsVO saveProductAndItemsWithoutUpdatingSolr(ProductAndItemsVO productAndItems);

  /**
   * Save saveProductAndItems with deferred reindex
   *
   * @param productAndItems
   * @return
   */
  ProductAndItemsVO saveProductAndItemsWithDeferredReindex(ProductAndItemsVO productAndItems);

  /**
   *
   * @param storeId
   * @param productSku
   * @param active
   * @param increment
   * @return
   */
  Product updateAndEvictOff2OnItemCountByProductSku(String storeId, String productSku, boolean active, int increment);

  List<Item> updateItemDGLevel(String storeId, Set<String> itemSku, Integer dangerousLevel);

  /**
   * @param storeId
   * @param itemSku
   * @return
   */
  Item updateItemFieldByItemSku(String storeId, String itemSku, String fieldName,
      Object fieldValue);

  /**
   * @param storeId
   * @param itemSku
   * @param fieldName
   * @param fieldValue
   * @param item
   * @return
   */
  ItemPickupPoint updateItemFieldByItemSkuAndPickupPoint(String storeId, String itemSku, String fieldName, Object fieldValue,
      Item item);

  /**
   * update item pickup point fields by item sku
   * @param storeId
   * @param itemSku
   * @param fieldName
   * @param fieldValue
   * @param item
   * @return
   */
  List<ItemPickupPoint> updateItemPickupPointFieldsByItemSku(String storeId, String itemSku, String fieldName,
      Object fieldValue, Item item);

  /**
   * Update merchantSku by itemSku
   *
   * @param storeId
   * @param itemSku
   * @param item
   * @param newMerchantSku
   * @return
   */
  List<ItemPickupPoint> updateMerchantSkuForItemPickupPoints(String storeId, String itemSku, Item item,
      String newMerchantSku);

  /**
   * @param storeId
   * @param itemSkus
   * @param fieldName
   * @param fieldValue
   * @return
   */
  List<Item> updateItemFieldByItemSkus(String storeId, Collection<String> itemSkus, String fieldName,
      Object fieldValue);

  /**
   * @param storeId
   * @param itemSku
   * @param active
   * @return
   */
  ProductAndItemsVO updateOff2OnChannelActiveByItemSku(String storeId, String itemSku,
      boolean active);

  ProductAndItemsVO updateOff2OnChannelActiveByProductSku(String storeId, String ProductSku,
      boolean active);

  /**
   * Save item in mongo and solr Databases and evict redis
   * @param item
   * @return
   */
  Item saveAndEvictItem(Item item);

  /**
   * Save items in mongo and solr Databases and evict redis
   * @param listOfItems
   * @return
   */
  List<Item> saveAndEvictItems(List<Item> listOfItems);

  /**
   * Update off2on flag by productSku
   *
   * @param storeId
   * @param productSku
   * @param active
   * @param userName
   * @param auditTrailResponseList
   *
   * @return
   */
  ProductAndItemsVO changeOff2OnChannelActiveByProductSkus(String storeId, String productSku,
      boolean active, String userName, List<AuditTrailDto> auditTrailResponseList, String requestId)  throws Exception;

  /**
   * update cnc active flag at l3, l4 and l5 level
   *
   * @param storeId
   * @param clientId
   * @param pickupPointItemMap
   * @param itemMap
   * @param itemPickupPointDataChangeEventModelList
   * @param productSkuAndPickupPointCodeMap
   * @param existingL4MadeCncFalse
   * @param l3MadeCncFalse
   */
  List<Product> updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(String storeId, String clientId,
      Map<String, List<ItemPickupPoint>> pickupPointItemMap, Map<String, Item> itemMap, Set<Item> itemsToReindex,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
      Map<String, Set<String>> productSkuAndPickupPointCodeMap, List<String> existingL4MadeCncFalse,
      List<String> l3MadeCncFalse);

  /**
   * Save items and clear cache without updating solr without publishing
   *
   * @param listOfItems
   * @return
   */
  List<ItemPickupPoint> saveItemPickupPointsAndClearCacheWithoutUpdatingSolrWithoutPublishing(
    List<ItemPickupPoint> listOfItems);
}
