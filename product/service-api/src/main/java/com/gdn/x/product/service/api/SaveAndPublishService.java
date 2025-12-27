package com.gdn.x.product.service.api;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.partners.merchant.voucher.streaming.model.VoucherItemSkusEventModel;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.OdooCreationEventModel;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.VideoCompressionEventModel;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;

public interface SaveAndPublishService {

  Item addActivePromoBundling(String storeId, String itemSku, String activePromoBundling);

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

  Item removeActivePromoBundling(String storeId, String itemSku, String activePromoBundling);

  /**
   * @param listOfItems must not be null
   * @return never null
   */
  List<Item> saveItems(List<Item> listOfItems);

  /**
   *
   * @param storeId
   * @param item
   * @return
   */
  void publishPristineItem(String storeId, Item item);

  /**
   * save product
   *
   * @param product
   * @param productChangeEventTypes
   * @param productPublishSourceDeletePickupPoint
   * @param eventToBlackListedSellersMap
   * @return
   */
  Product saveProduct(Product product, List<String> productChangeEventTypes,
    String productPublishSourceDeletePickupPoint,
    Map<String, Set<String>> eventToBlackListedSellersMap);

  /**
   * @param product
   * @return
   */
  Product saveProductAndSKipPublishForMfdTrueProducts(Product product);

  /**
   * Save list of products
   *
   * @param productList
   * @return
   */
  List<Product> saveProducts(List<Product> productList);

  /**
   * @param storeId
   * @param itemSku
   * @param fieldName
   * @param fieldValue
   * @return
   */
  Item updateItemFieldByItemSku(String storeId, String itemSku, String fieldName,
      Object fieldValue);

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
  Item updateItemOff2OnChannelActiveByItemSku(String storeId, String itemSku, boolean active);

  /**
   * @param storeId
   * @param productSku
   * @param active
   * @return
   */
  List<Item> updateItemOff2OnChannelActiveByProductSku(String storeId, String productSku,
      boolean active);

  /**
   * Update flag without publishing events
   *
   * @param storeId
   * @param productSku
   * @param active
   * @param userName
   * @return
   */
  List<Item> updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(String storeId, String productSku,
      boolean active, String userName);

  Product updateOff2OnItemCountIncrement(String storeId, String productSku, boolean activate,
      int increment);

  /**
   * Update productSku off2on and flag
   *
   * @param storeId
   * @param productSku
   * @param activate
   * @param itemsCount
   * @param userName
   * @return
   */
  Product updateOff2OnItemCountIncrementAndFlag(String storeId, String productSku, boolean activate, int itemsCount,
      String userName);

  /**
   * publish items to X-search using ITEM_CHANGE_EVENT_NAME event
   * @param items
   */
  void publishListOfItems(List<Item> items);

  /**
   * publish items to X-search using ITEM_CHANGE_EVENT_NAME event for halal config change
   * @param items
   * @param halalProduct
   */
  void publishListOfItemsForHalaConfigChange(List<Item> items, boolean halalProduct);

  /**
   * Publish list of items
   *
   * @param items
   * @param itemPickupPoints
   * @param itemPickupPointDataChangeEventModelList
   * @param productRejected
   */
  void publishListOfItems(List<Item> items, List<ItemPickupPoint> itemPickupPoints,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList, boolean productRejected);

  /**
   * Publish items with source
   *
   * @param items
   * @param itemPickupPoints
   * @param itemPickupPointDataChangeEventModelList
   * @param source
   * @param eventToBlackListedSellersMap
   */
  void publishListOfItems(List<Item> items, List<ItemPickupPoint> itemPickupPoints,
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
    String source, boolean productRejected, Map<String, Set<String>> eventToBlackListedSellersMap);

  /**
   * Publish list of items with item change type
   *
   * @param items
   * @param itemChangeEventTypeList
   */
  void publishListOfItemsWithItemChangeType(List<Item> items, List<ItemChangeEventType> itemChangeEventTypeList);

  /**
   * Publish product change event
   * @param product
   * @param productChangeEventTypes
   */
  void publishProduct(Product product, List<String> productChangeEventTypes);

  void publishListOfOfflineItems(List<OfflineItem> offlineItems, MandatoryRequestParam mandatoryRequestParam);

  /**
   * publish ItemPickupPoints
   *
   * @param pickupPointList
   * @param mandatoryRequestParam
   */
  void publishListOfItemsPickupPoints(List<ItemPickupPoint> pickupPointList, MandatoryRequestParam mandatoryRequestParam);

  void publishListOfOfflineItemsWithoutFailsafe(List<OfflineItem> offlineItems) throws Exception;

  /**
   * publish products to kafka using PRODUCT_CHANGE_EVENT_NAME event
   * @param productStream
   */
  void publishStreamOfProducts(Stream<Product> productStream);

  /**
   * publish items to kafka using ITEM_CHANGE_EVENT_NAME event
   * @param itemStream
   */
  void publishStreamOfItems(Stream<Item> itemStream);

  /**
   * Publish Merchant's Promo discount price
   * @param item
   */
  void publishMerchantPromoDiscountEventChange(Item item);

  /**
   * Publish event to voucher
   * @param voucherItemSkusEventModel
   */
  void publishItemSkuListForVoucher(VoucherItemSkusEventModel voucherItemSkusEventModel);

  /**
   * Publish item view Config change event to voucher
   * @param itemVo
   */
  void publishMerchantVoucherViewConfigChange(ItemVo itemVo);

  /**
   * Publish item view Config change event to voucher
   * @param itemPickupPoints
   * @param items
   */
  void publishMerchantVoucherViewConfigChange(List<ItemPickupPoint> itemPickupPoints, List<Item> items);

  /**
   * Publish wholesale price activated or deactivated event to PBP history update
   * @param itemSku
   * @param wholesalePriceActivated
   * @param merchantCode
   */
  void publishWholesalePriceActivatedOrDeactivatedEvent(String itemSku, boolean wholesalePriceActivated,
      String merchantCode);

  /**
   * Publish event with list of product sku for reindex
   *
   * @param productSkuList
   */
  void publishProductL3SolrReindexEvent(List<String> productSkuList, String status);

  /**
   * @param itemVo
   * @return
   */
  ItemChange publishItemChangeEvent(ItemVo itemVo, ItemPickupPointVo itemPickupPointVo);

  /**
   * publish item change event on mpp off
   * @param itemPickupPointList
   * @param itemMap
   */
  void publishItemChangeEvent(List<ItemPickupPoint> itemPickupPointList, Map<String, Item> itemMap);

  /**
   * publish item data change event L4
   *
   * @param items
   */
  void publishItemDataChangeEvent(List<Item> items);

  /**
   * Publish L4 events with source of the publish
   *
   * @param items
   * @param source
   * @param productRejected
   */
  void publishItemDataChangeEvent(List<Item> items, String source, boolean productRejected);

  /**
   * publish item data change event L4
   * @param item
   * @param halalProduct
   */
  void publishItemDataChangeEventForHalalConfigChange(Item item, boolean halalProduct);

  /**
   * publish item pickup point data change event L5
   *
   * @param itemPickupPoints
   * @param itemPickupPointChangeEventTypes
   * @param eventToBlackListedSellersMap
   */
  void publishItemPickupPointDataChangeEvent(List<ItemPickupPoint> itemPickupPoints,
      List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes,
    Map<String, Set<String>> eventToBlackListedSellersMap);

  /**
   * publish iffline item change event using item pickup point
   *
   * @param itemPickupPoints
   * @param itemMap
   * @param clientId
   * @param itemPickupPointDataChangeEventModelList
   */
  void publishListOfOfflineItems(List<ItemPickupPoint> itemPickupPoints, Map<String, Item> itemMap, String clientId,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList);

  /**
   * Publish list of items for L5 Migration
   *  @param itemSkuList
   *
   */
  void publishItemsForMigration(List<String> itemSkuList);

  /**
   * Publish event to add entry for migration with state
   *  @param itemSkuList
   */
  void publishItemCreationForMigration(List<String> itemSkuList);

  /**
   * Publish item view Config change event to voucher
   * @param itemPickupPoint
   */
  void publishViewConfigChange(ItemPickupPoint itemPickupPoint);

  /**
   *Publish Item and Pickup Point Data Change To AGP
   * @param itemPickupPoint
   * @return
   */
  ItemPickupPointDataChangeEventModel publishItemPickupPointDataChangeEventForAGP(ItemPickupPoint itemPickupPoint);

  /**
   * Publish Merchant's Promo discount price with pick up point code
   * @param itemPickupPoint
   */
  void publishMerchantPromoDiscountEventChange(ItemPickupPoint itemPickupPoint);

  /**
   * Publish solr update event
   * @param productAndItemEventModels
   */
  void publishSolrUpdateEvent(List<ProductAndItemEventModel> productAndItemEventModels);

  /**
   * @param offlineItemChange
   */
  void publishOfflineItemChangeSellerEvent(OfflineItemChange offlineItemChange);

  /**
   * publish event for the item that have only one sku in bundle recipe
   * @param items
   */
  void publishProductBundleOneToOneMappingEvent(Collection<Item> items);

  /**
   *
   * @param auditTrailDtoList
   */
  void publishHistoryEvent(List<AuditTrailDto> auditTrailDtoList);

  /**
   * publish odoo creation event
   *
   * @param odooCreationEventModel
   */
  void publishOdooCreationEvent(OdooCreationEventModel odooCreationEventModel);
  /**
   * publish video compression event
   *
   * @param videoCompressionEventModel video compression model
   */
  void publishVideoCompressionEvent(VideoCompressionEventModel videoCompressionEventModel);
}