package com.gdn.x.product.service.api;

import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.partners.product.pricing.streaming.model.promo.bundling.ItemInfo;
import com.gdn.x.product.domain.event.model.MasterSkuMappingEventModel;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.vo.BundleRecipeRequest;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.ProductAndItemPickupPointDTO;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemBasicL4Response;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;

import com.gdn.x.product.rest.web.model.response.UpcStatusResponse;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.partners.merchant.voucher.streaming.model.VoucherItemSkusEventModel;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingActivatedDeactivatedEventModel;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.vo.DefaultItemSkuVO;
import com.gdn.x.product.model.vo.ItemAndBundlingInfoVO;
import com.gdn.x.product.model.vo.ItemAndItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductTypeEditRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;

public interface ItemService {

  /**
   * method for publish all items
   * @param storeId must not be blank
   */
  void publishAllItems(String storeId);

  /**
   * method for publish items to AGP
   * @param storeId must not be blank
   * @param itemSkus must not be empty
   */
  void republishItemsToAgp(String storeId, List<String> itemSkus);

  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username must not be blank
   * @param productSku must not be blank
   * @param itemRequestVO must not be null
   * @return boolean
   * @throws Exception
   */
  boolean addItem(String storeId, String requestId, String username, String productSku,
      Item itemRequestVO) throws Exception;

  /**
   * @param storeId must not be blank
   * @param price must not be null
   * @param itemSku must not be blank
   * @param username must not be blank
   * @return boolean
   */
  boolean addItemPrice(String storeId, Price price, String itemSku, String username);

  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username is optional
   * @param merchantSku can be blank
   * @param merchantCode must not be blank
   * @param price must not be null
   * @return boolean
   */
  boolean addItemPriceByMerchantSkuAndMerchantCode(String storeId, String requestId,
      String username, String merchantSku, String merchantCode, Price price);

  /**
   * add product and item details while creating the product
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param productSku
   * @param listOfItemRequestVO
   * @param product
   * @param productDetailResponse
   * @param businessPartner
   * @return
   * @throws Exception
   */
  List<ItemVo> addItems(String storeId, String requestId, String username, String productSku,
      List<ItemVo> listOfItemRequestVO, Product product,
      ProductDetailResponse productDetailResponse, BusinessPartner businessPartner) throws Exception;

  Item addActivePromoBundling(String storeId, String itemSku, String promoBundlingType);

  /**
   * @param storeId must not be blank
   * @param pristineDataItem must not be null
   * @return itemCodes
   */
  Set<String> getItemCodesByPristine(String storeId, PristineDataItem pristineDataItem);

  Item removeActivePromoBundling(String storeId, String itemSku, String promoBundlingType);

  /**
   * Activate or decativate wholesale price or promo
   *
   * @param storeId
   * @param itemSku
   * @param promoBundlingType
   * @param promoBundlingActivated
   * @param wholesalePriceActivated
   * @return
   */
  void processPromoBundlingStatusChangedEvent(String storeId, String itemSku, String promoBundlingType,
      boolean promoBundlingActivated, Boolean wholesalePriceActivated);

  /**
   * Activate or decativate wholesale price or promo
   *
   * @param storeId
   * @param itemSku
   * @param promoBundlingType
   * @param promoBundlingActivated
   * @param wholesalePriceActivated
   */
  void processPromoBundlingStatusChangedEventInItemPickupPoint(String storeId, String itemSku, String promoBundlingType,
      boolean promoBundlingActivated, Boolean wholesalePriceActivated);

  /**
   * Activate or decativate wholesale price or promo
   *
   * @param storeId
   * @param itemSku
   * @param promoBundlingType
   * @param promoBundlingActivated
   * @param wholesalePriceActivated
   * @return
   */
  ItemPickupPoint processPromoBundlingStatusChangedEventInItemPickupPointAndPPCode(String storeId, String itemSku, String promoBundlingType,
      boolean promoBundlingActivated, Boolean wholesalePriceActivated, String ppCode);

  List<Item> assignTicketTemplateToItems(String storeId, List<String> itemSkus,
      String ticketTemplateCode);

  /**
   * @param storeId must not be blank
   * @param itemSku must not be blank
   * @return false if failed
   * @throws Exception if failed
   */
  boolean deleteItem(String storeId, String itemSku) throws Exception;

  /**
   * @param storeId   must not be blank
   * @param itemSku   must not be blank
   * @param doArchive
   * @return EditItemResponse
   * @throws Exception if failed
   */
  EditItemResponse toggleArchiveItem(String storeId, String itemSku, String username, boolean doArchive) throws Exception;

  /**
   * @param storeId must not be blank
   * @param itemSku must not be blank
   * @param channel must not be null
   * @return failed if success
   * @throws Exception if fail
   */
  boolean deleteItemPrice(String storeId, String itemSku, String channel) throws Exception;

  /**
   * Delete Item By StoreId And ProductSkus
   *
   * @param storeId
   * @param productSkus
   * @return
   */
  List<Item> deleteItemByStoreIdAndProductSkus(String storeId, Set<String> productSkus);

  Item findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku);

  Item findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(String storeId, String itemSku);

  /**
   * Find item with itemSku
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  Item findByStoreIdAndItemSku(String storeId, String itemSku);

  List<Item> findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId,
      String productSku);

  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username must not be blank
   * @param itemSku must not be blank
   * @return item
   * @throws Exception
   */
  Item getItem(String storeId, String requestId, String username, String itemSku,
      boolean needMasterDataDetail) throws Exception;

  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username must not be blank
   * @param itemSku must not be blank
   * @param needMasterDataDetail
   * @param quickDetail
   * @param combineOthersBundlings
   * @param instantPickup
   * @param pickupPointCode
   * @param off2On
   * @param fetchMfdTrueItem
   * @return item
   * @throws Exception
   */
  Item getItem(String storeId, String requestId, String username, String itemSku, boolean needMasterDataDetail,
      boolean quickDetail, boolean combineOthersBundlings, boolean instantPickup, String pickupPointCode,
      boolean off2On, boolean fetchMfdTrueItem) throws Exception;

  /**
   * get Item details by adding the L5 data
   *
   * @param storeId
   * @param itemSku
   * @return
   * @throws Exception
   */
  ItemAndItemPickupPointVo getItemDetails(String storeId, String itemSku, String pickupPointCode);

  /**
   *
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return
   */
  ItemAndItemPickupPointVo getItemDetailsFromDB(String storeId, String itemSku, String pickupPointCode);

  /**
   * get Item details by adding the L5 data
   *
   * @param storeId
   * @param itemSku
   * @return
   * @throws Exception
   */
  ItemAndItemPickupPointVo getItemAndPickupPointDetails(String storeId, List<String> productSkus, List<String> itemSku,
      boolean showDeleted, int page, int pageSize);

  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username must not be blank
   * @param itemSku must not be blank
   * @param needMasterDataDetail
   * @param quickDetail
   * @param combineOthersBundlings
   * @param instantPickup
   * @param pickupPointCode
   * @param off2On
   * @return item
   * @throws Exception
   */
  Item getDetailsForActiveOrSuspendedItem(String storeId, String requestId, String username, String itemSku,
      boolean needMasterDataDetail, boolean quickDetail, boolean combineOthersBundlings, boolean instantPickup,
      String pickupPointCode, boolean off2On) throws Exception;

  Map<String, List<Item>> getItemAvailability(String storeId, Set<String> productSkus);

  List<Item> getItemPriceAndOff2OnChannelActive(String storeId, List<String> itemSkus);

  /**
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> getItemPriceAndViewConfigs(String storeId, List<String> itemSkus);

  /**
   * @param storeId must not be blank
   * @param merchantSku must not be blank
   * @param merchantCode must not be blank
   * @return list of items
   */
  List<Item> getItemsByMerchantSkuAndMerchantCode(String storeId, String merchantSku,
      String merchantCode);

  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @return items
   */
  List<Item> getItemsByProductSku(String storeId, String productSku);

  /**
   * @param storeId                 must not be blank
   * @param requestId               must not be blank
   * @param username                must not be blank
   * @param productSku              must not blank
   * @param isSynchronized
   * @param level2MerchantCList     must not be blank
   * @param showDeleted
   * @param combineOthersBundlings
   * @param off2On
   * @param isMigrateAndSyncProduct
   * @return empty list if no item found
   * @throws Exception if fail
   */
  List<Item> getItemsForViewByProductSku(String storeId, String requestId, String username, String productSku,
      boolean isSynchronized, String level2MerchantCList, boolean showDeleted, boolean combineOthersBundlings,
      boolean off2On, boolean isMigrateAndSyncProduct) throws Exception;

  /**
   *
   * returns item details with product sku and pickup point code
   *
   * @param storeId
   * @param productSku
   * @param showDeleted
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  List<Item> getItemsForViewByProductSkuAndPickUpPoint(String storeId, String productSku, boolean showDeleted,
      boolean combineOthersBundlings, boolean off2On, Map<String, ItemPickupPoint> itemPickupPointMap) throws Exception;

  /**
   * update items with discount and pick-up point details
   * @param storeId
   * @param productSku
   * @param pickupPointCode
   * @param items
   * @param itemPickupPoints
   * @param validateViewConfig
   * @return
   */
  List<Item> getItemsWithDiscountAndPickUpPointDetails(String storeId,
      String productSku, String pickupPointCode, List<Item> items, List<ItemPickupPoint> itemPickupPoints,
      Map<String, ItemPickupPoint> itemPickupPointMap, boolean validateViewConfig);

  /**
   * @param storeId must not be blank
   * @param username must not be blank
   * @param requestId must not be blank
   * @param productSkus must not null
   * @param combineOthersBundlings must not be null
   * @param off2On
   * @return empty list if no item found
   */
  List<Item> getItemsWithDiscountPriceByProductSkus(String storeId, String username, String requestId,
      Set<String> productSkus, boolean combineOthersBundlings, boolean off2On);

  void updateDangerousGoodsLevel(String storeId, Map<Integer, Set<String>> itemSku)
      throws Exception;

  void updateDangerousGoodsLevel(String storeId, Set<String> itemSkus, Integer dangerousLevel)
      throws Exception;

  boolean updateEtdNote(String storeId, String itemSku, String etdNote);

  /**
   * @param storeId must not be blank
   * @param item must not be null
   * @param username must not be null
   * @param isOnlyExternal
   * @param isProductTypeChanged
   * @param isPreOrderChanged
   * @return boolean
   * @throws Exception if fail
   */
  Item updateItem(String storeId, Item item, String username, boolean isOnlyExternal, boolean isProductTypeChanged,
      boolean isPreOrderChanged) throws Exception;

  /**
   *
   * @param itemToSet
   * @param updatedPrices
   * @param username
   * @return
   */
  boolean validatePriceChangeAndSet(Item itemToSet, Set<Price> updatedPrices, String username);

  /**
   * @param username must not be blank
   * @param storeId must not be blank
   * @param price must not be null
   * @param itemSku must not be blank
   * @param wholeSalePriceExists
   * @return boolean
   * @throws Exception if channel not found
   */
  boolean updateItemPrice(String username, String storeId, Price price, String itemSku, Boolean wholeSalePriceExists)
      throws Exception;

  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username must not be blank
   * @param merchantSku can be blank
   * @param merchantCode must not be blank
   * @param price must not be null, channel inside must be specified
   * @return boolean
   */
  boolean updateItemPriceByMerchantSkuAndMerchantCode(String storeId, String requestId,
      String username, String merchantSku, String merchantCode, Price price);


  /**
   * Update salesCatalog for pristineItems by pristineIds
   * @param pristineIds
   * @return
   */
  boolean updateSalesCatalogForPristineProducts(List<String> pristineIds);

  /**
   *  get all items by pristineIds
   * @param storeId must not be blank
   * @param pristineIds must not null
   * @return empty list if no item found
   */
  List<Item> getItemsByPristineIds(String storeId, Set<String> pristineIds) throws Exception;

  /**
   *  get all items by pristineIds
   * @param storeId must not be blank
   * @param pristineIds must not null
   * @return empty list if no item found
   */
  List<Item> getAllItemsByPristineIds(String storeId, Set<String> pristineIds) throws Exception;

  /**
   *  get all item SKUs by pristineIds
   * @param storeId must not be blank
   * @param pristineIds must not null
   * @return empty list if no item found
   */
  List<Item> getItemSkusByPristineIdsAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(String storeId,
      Set<String> pristineIds) throws Exception;

  /**
   *  get all items by productSkus
   * @param storeId must not be blank
   * @param productSkus must not null
   * @return empty list if no item found
   */
  List<Item> getItemsByProductSkus(String storeId, Set<String> productSkus) throws Exception;

  /**
   * return map for attribute map for pristine product
   *
   * @return
   */
  Map<String, String> getMapForPristineCategoryAttribute();

  /**
   * get the itemSkus by productCode and publish it to search
   * @param storeId must not be blank
   * @param itemsChanged
   * @param skuCodeNeedToPublish
   * @param productDomainEventModel
   */
  void publishItemSkus(String storeId, Map<String, Boolean> itemsChanged,
      Set<String> skuCodeNeedToPublish, ProductDomainEventModel productDomainEventModel);

  /**
   * Archive and delete items
   *
   * @param productDomainEventModel
   * @param eventToBlackListedSellersMap
   */
  void archiveAndDeleteActiveProduct(ProductDomainEventModel productDomainEventModel,
    Map<String, Set<String>> eventToBlackListedSellersMap);

  /**
   * Find buyable and discoverable items by pristineId
   * @param storeId
   * @param pristineId
   * @return
   */
  List<Item> findBuyableDiscoverableItemsByPristineId(String storeId, String pristineId);

  /**
   *  Find all items by pristineId
   * @param storeId
   * @param pristineId
   * @return
   */
  List<Item> getItemsByPristineId(String storeId, String pristineId);

  /**
   * Check if the item is buyable and discoverable
   * @param item
   * @return
   */
  Boolean isItemBuyableAndDiscoverable(Item item);

  /**
   * Find first buyable discoverable item sku
   *
   * @param storeId    must not be blank
   * @param itemSkuSet i.e. sorted set of item skus as per buyBoxScores
   * @param pristineId i.e. pristineId
   * @return
   */
  DefaultItemSkuVO findFirstBuyableDiscoverableItemSkuByPristineId(String storeId, LinkedHashSet<String> itemSkuSet,
      String pristineId);

  /**
   * @param storeId must not be blank
   * @param username must not null
   * @param requestId must not null
   * @param pristineMasterIds must not null
   * @return empty list if no item found
   */
  List<Item> getItemsWithDiscountPriceByPristineMasterIds(String storeId, String username,
      String requestId, Set<String> pristineMasterIds);

  /**
   * get item catalog for given pristine id
   *
   * @param pristineId
   * @return
   * @throws Exception
   */
  List<ItemCatalogVO> getItemCatalogsByPristineId(String pristineId) throws Exception;

  /**
   * get maximum sequence from all categories
   *
   * @param itemCatalogVOs category Hierarchy
   * @return
   * @throws Exception
   */
  List<SalesCategorySequence> getSalesCategorySequenceListFromCategoryHierarchy(List<ItemCatalogVO> itemCatalogVOs)
      throws Exception;

  /**
   * Update Pristine Data in X-product's prd_item collection by itemCode
   * @param storeId
   * @param itemSkuPristineDataItemsMap itemCode to PristineDataMap
   */
  void updateItemsPristineData(String storeId,
      Map<String, PristineDataItem> itemSkuPristineDataItemsMap)
      throws Exception;



  /**
   * Update Pristine Data in X-product's prd_item collection by itemCode
   * @param pristineDataItem
   */
  void updateItemsPristineDataByItemCode(PristineDataItem pristineDataItem) throws Exception;

  /**
   * Update resign merchant items to archive, buyable & discoverable false by merchant code
   * @param storeId
   * @param requestId
   * @param username
   * @param merchantCode
   */
  void updateResignMerchantItemsByMerchantCode(String storeId, String requestId, String username,
      String merchantCode);

  /**
   * Get Items by Merchant Code and List of Merchant SKU
   * @param storeId must not blank
   * @param requestId must not blank
   * @param username must not blank
   * @param merchantCode must not blank
   * @param merchantSkus must not null
   */
  List<Item> getItemsByMerchantCodeAndMerchantSkus(String storeId, String requestId, String username,
      String merchantCode, List<String> merchantSkus);

  /**
   * Update merchant items promoBundling
   * @param storeId
   * @param itemSkuSet
   * @param promoBundling
   */
  void updatePromoBundlingByItemSkus(String storeId, Set<String> itemSkuSet, boolean promoBundling);

  /**
   * Update promo bundling flag
   *
   * @param storeId
   * @param itemSkus
   * @param promoBundling
   */
  void updatePromoBundlingByItemSkusInItemPickupPoint(String storeId, Set<String> itemSkus, boolean promoBundling);


  /**
   * Update promo bundling flag by L4 and PP code
   *
   * @param storeId
   * @param itemInfos
   * @param promoBundling
   */
  void updatePromoBundlingByItemSkusInItemPickupPointByItemInfo(String storeId, List<ItemInfo> itemInfos,
      boolean promoBundling);

  /**
   *  get all items by pristineMasterId
   * @param storeId must not be blank
   * @param username must not null
   * @param requestId must not null
   * @param pristineMasterId must not null
   * @return empty list if no item found
   */
  List<Item> getItemsByPristineMasterId(String storeId, String username,
      String requestId, String pristineMasterId);

  /**
   * @param storeId
   * @param productSku
   * @return list of pristine Ids by store id and product sku
   */
  List<String> getPristineIdsByProductSku(String storeId, String productSku);

  /**
   * updates sales catalog for all pristine products by list of pristineIds
   * @param pristineIds must not be null
   * @return boolean
   */
  boolean updateSalesCatalogForAllPristineProducts(List<String> pristineIds);

  /**
   * get map of item code by item sku (itemSku as key, itemCode as value) of itemSkus specified in parameter
   * @param storeId must not be blank
   * @param itemSkus must not be empty
   * @return map of item code by item sku
   */
  Map<String, String> getItemCodesByItemSkuIn(String storeId, Set<String> itemSkus);

  /**
   * get map of items by item sku (itemSku as key, Item as value) of itemSkus specified in parameter
   * @param storeId must not be blank
   * @param itemSkus must not be empty
   * @return map of items by item sku
   */
  Map<String, Item> getItemsByItemSkuIn(String storeId, Set<String> itemSkus);

  Item updateCncActivated(String storeId, String itemSku, boolean cncActivated, String username);

  /**
   * updating categoryCode in item by itemSkus
   *
   * @param storeId
   * @param itemSkus
   * @param categoryCode
   */
  void updateCategoryCodeByItemSkuList(String storeId, List<String> itemSkus, String categoryCode);

  void updateCncActivatedByMerchantCode(String storeId, String merchantCode, boolean cncActivated, String username);

  /**
   * update cncActivated by itemSku
   *
   * @param storeId
   * @param itemSkuSet
   * @param cncActivated
   * @param username
   */
  void updateCncActivatedByItemSkusAndPublish(String storeId, Set<String> itemSkuSet, boolean cncActivated, String username) throws Exception;

  List<Item> getItemsByProductSkuAndCncActivated(String storeId, String username, String requestId,
      String productSku, boolean cncActivated);

  /**
   * @param storeId must not be blank
   * @param itemCode must not be blank
   * @return
   */
  Item getItemByItemCode(String storeId, String itemCode);

  /**
   * @param storeId must not be blank
   * @param pristineId must not be blank
   * @return Set of string itemCodes
   * @throws Exception
   */
  Set<String> getItemCodesByPristineId(String storeId, String pristineId) throws Exception;

  ItemAndBundlingInfoVO getItemsAndBundlingInfo(String storeId, String channelId, String clientId,
      String requestId, Set<String> itemSkus, Set<String> promoBundlingIds, String username)
      throws Exception;

  List<Item> findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
      String storeId, String itemCode);

  /**
   * get all items mapped to pristine ID of itemCode
   * get only items which are buyable, discoverable
   *
   * @param storeId store id, defalut = 10001
   * @param username username
   * @param requestId request Id
   * @param itemCode item code for which need all SKU
   * @param fetchAll
   * @return list of items
   * @throws Exception
   */
  List<ItemPriceVO> getAllItemSkuByItemCode(String storeId, String username, String requestId, String itemCode,
      boolean fetchAll);

  /**
   * update discount price (Promotion Price) for item skus
   * @param storeId
   * @param itemSkus
   */
  void updatePromotionPriceForItemSkuList(String storeId, Set<String> itemSkus);

  /**
   * fetch values from cache first, and if some values not present for them fetch from DB
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> findByStoreIdAndItemSkus(String storeId, Set<String> itemSkus);


  /**
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> findByStoreIdAndItemSkusReadFromPrimary(String storeId, Set<String> itemSkus);


  /**
   * get items by itemSku not cached
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> findByStoreIdAndItemSkusNotCached(String storeId, Set<String> itemSkus);

  /** Update the flash sale active flag for item
   * @param storeId
   * @param itemSkus
   * @param isFlashSaleActive
   * @return
   */
  boolean updateItemFlashSaleActiveFlag(String storeId, List<String> itemSkus, boolean isFlashSaleActive);
  /**
   * Get Items with limited fields by productSkus and combineOthersBundlings
   * @param storeId
   * @param username
   * @param requestId
   * @param productSkus
   * @param combineOthersBundlings
   * @return
   */
  List<Item> findByStoreIdAndMarkForDeleteFalseAndProductSkus(String storeId, String username,
      String requestId, Set<String> productSkus, boolean combineOthersBundlings);

  /**
   * Get item by updated by and send event to search
   * @param storeId
   * @param updatedBy
   */
  void sendItemChangeEventByUpdatedBy(String storeId, String updatedBy);

  /**
   * Update Merchant's Promo discount price
   * @param storeId
   * @param item
   * @return item
   */
  void updateItemMerchantDiscountPrice(String storeId, Item item) throws Exception;

  /**
   * Publish event and clear cache without updating L4 db
   *
   * @param storeId
   * @throws Exception
   */
  void updateItemMerchantDiscountPriceWithoutUpdatingL4Db(String storeId, String itemSku, DiscountPrice discountPrice)
      throws Exception;


  /**
   * Update discount price by item sku and pp code
   *
   * @param storeId
   * @param itemSku
   * @param discountPrice
   * @param ppCode
   * @param activated
   * @throws Exception
   */
  void updateItemMerchantDiscountPriceByItemSkuAndPPCode(String storeId, String itemSku, DiscountPrice discountPrice,
      String ppCode, boolean activated) throws Exception;

  /**
   * Update Merchant Promo Discount Active/Pending Flag : This item is mapped to promo discount or not
   * @param storeId
   * @param itemSku
   * @param isPromoActive
   * @return Item
   */
  void updateMerchantPromoDiscountFlag(String storeId, String itemSku, boolean isPromoActive) throws Exception;


  /**
   * Update MPD flag in item pickup point
   *
   * @param storeId
   * @param itemSku
   * @param isPromoActive
   * @throws Exception
   */
  void updateMerchantPromoDiscountFlagInItemPickupPoint(String storeId, String itemSku, boolean isPromoActive) throws Exception;


  /**
   * Update MPD flag in item pickup point
   *
   * @param storeId
   * @param itemSku
   * @param isPromoActive
   * @param ppCode
   * @throws Exception
   */
  void updateMerchantPromoDiscountFlagByItemSkuAndPPCode(String storeId, String itemSku, boolean isPromoActive, String ppCode) throws Exception;


  /**
   * Get updated items
   *
   * @param storeId
   * @param lastIndexTime
   * @param pageable
   * @return
   */
  Page<Item> findByStoreIdAndUpdatedDateGreaterThan(String storeId, Date lastIndexTime, Pageable pageable);

  /**
   * Get and Publish all item sku to merchant voucher
   * @param voucherItemSkusEventModel
   */
  void publishItemsByMerchantCodeToVoucher(VoucherItemSkusEventModel voucherItemSkusEventModel);

  /**
   * API to suspend and activate the item.
   *
   * @param storeId
   * @param productSku
   * @param suspendItem
   */
  Pair<Map<String, Boolean>, List<Item>> suspendItems(String storeId, String productSku, boolean suspendItem) throws Exception;

  /**
   *
   * @param item
   * @return
   */
  boolean isPriceEditDisabled(Item item);

  /**
   * @param itemPickupPoint
   * @return
   */
  boolean isPriceEditDisabled(ItemPickupPoint itemPickupPoint);

  /**
   * Update subscription flag for given itemSku
   *  @param storeId
   * @param itemSku
   * @param subscriptionFlag
   * @param preferredSubscriptionType
   */
  void updateSubscriptionFlagByItemSku(String storeId, String itemSku, boolean subscriptionFlag,
    Set<String> preferredSubscriptionType);

  /**
   * get items with mfd false from itemcode
   *
   * @param storeId
   * @param itemCode
   * @return
   */
  List<Item> getItemsByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId, String itemCode);

  /**
   * get items with discount
   *  @param storeId
   * @param username
   * @param requestId
   * @param items
   */
  List<Item> getItemsWithDiscountPrice(String storeId, String username,String requestId, List<Item> items);

  /**
   * Update pristine DPC
   *
   * @param itemSku
   * @param productCode
   * @param pristineMasterId
   */
  void updatePristineDPC(String itemSku, String pristineMasterId, String productCode);

  /**
   * Fetch all items for product sku
   *
   * @param storeId
   * @param productSku
   */
  List<Item> findItemsByStoreIdAndProductSku(String storeId, String productSku);

  /**
   * Find productSku to List of items map
   *
   * @param storeId
   * @param productSkuList
   * @return
   */
  Map<String, ProductAndItemsVO> getProductAndItemsMap(String storeId, List<String> productSkuList);

  /**
   * Fetch list of items from cache by product sku. If not loaded in cache then fetch from db
   *
   * @param storeId
   * @param productSku
   * @return
   */
  List<Item> getItemsByProductSkuFromCacheOrElseDB(String storeId, String productSku);

  /**
   * update pickup point
   *
   * @param storeId
   * @param pickupPointUpdateRequest
   * @return
   */
  ProductAndItemPickupPointDTO updatePickupPoints(String storeId,
    PickupPointUpdateRequest pickupPointUpdateRequest, boolean readFromPrimary) throws Exception;

  /**
   * update pickupPointCode in itemPickupPoint
   *
   * @param pickupPoint
   * @param itemPickupPoint
   * @param item
   * @param pickupPointCodeToPureCNCStatusChangeMap
   * @param isCombinedEditRequest
   * @param editProductDetailDTO
   * @return
   */
  Map<String, ItemPickupPoint> updatePickupPointInItemPickupPoints(String pickupPoint, ItemPickupPoint itemPickupPoint,
      Item item, Map<String, Boolean> pickupPointCodeToPureCNCStatusChangeMap, boolean isCombinedEditRequest,
      EditProductDetailDTO editProductDetailDTO, boolean readFromPrimary) throws Exception;

  /**
   *
   * @param storeId
   * @param ItemSkus
   * @return
   */
  List<Item> getItemPriceAndViewConfigsAndPromoDetails(String storeId, List<String> ItemSkus);

  EditItemResponse doArchivalAction(Item item, String storeId, String username, boolean doArchive,
      List<ItemPickupPoint> itemPickupPointList, boolean isRejected);

  /**
   * API to archive all the items of a product sku
   *
   * @param storeId must not be blank
   * @param username
   * @param productSku must not be blank
   * @param doArchive
   * @throws Exception if failed
   * @return
   */
  EditItemResponse toggleArchiveByProductSku(String storeId, String username, String productSku,
      boolean doArchive, String source) throws Exception;

  /**
   * get pickup point for items in paginated form
   * @param storeId
   * @param productSku
   * @param page
   * @param size
   * @return
   */
  Page<Item> getItemsByProductSkuPaginated(String storeId, String productSku, int page, int size);


  /**
   * update content change
   * @param storeId
   * @param productSku
   * @param contentChange
   * @param publishItems
   */
  void updateContentChange(String storeId, String productSku, boolean contentChange, boolean publishItems);

  /**
   * Get items by itemSkus
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  Map<String, String> findListOfItemCodesByItemSkus(String storeId, Set<String> itemSkus);

  /**
   * @param storeId
   * @param productSkus
   * @param pageRequest
   * @return
   */

  Page<Item> getItemsByProductSkusPaginated(String storeId, String productSkus, PageRequest pageRequest);

  /**
   * @param storeId
   * @param itemSku
   * @return
   */
  Item findByStoreIdAndItemSkuFromCache(String storeId, String itemSku);

  /**
   * api to activate item for need revision
   *
   * @param storeId
   * @param productSku
   * @param itemActivationRequests
   * @return
   */
  List<Item> activateItemsOnNeedCorrection(String storeId, String productSku,
      List<NeedCorrectionItemActivationRequest> itemActivationRequests) throws Exception;

  /**
   * api to activate item for need revision with mpp
   *
   * @param storeId
   * @param productSku
   * @param merchantCode
   * @param itemActivationRequests
   * @param isFreeSampleOrOfflineProduct
   * @return
   */
  EditItemResponse activateItemsOnNeedCorrectionWithMpp(String storeId, String productSku, String merchantCode,
      List<NeedCorrectionItemActivationRequest> itemActivationRequests, boolean isFreeSampleOrOfflineProduct) throws Exception;

  /**
   * Find items by itemSkus and markForDelete false from database
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> getItemsByStoreIdAndItemSkus(String storeId, Set<String> itemSkus);

  /**
   * Get items by productSkus or ItemSkus
   *
   * @param storeId
   * @param productSkus
   * @param itemSkus
   * @return
   */
  List<Item> getItemsByStoreIdAndProductSkusOrItemSkusIn(String storeId, List<String> productSkus, List<String> itemSkus);

  /**
   * Get items by itemSkus with required fields
   *
   * @param storeId
   * @param itemSkus
   * @param fields
   * @return
   */
  List<Item> getItemsByItemSkus(String storeId, Set<String> itemSkus, String[] fields);

  /**
   * Fetch items by offline item ids
   *
   * @param storeId
   * @param itemSkusForOfflineItemSearch
   * @param setOfflineItemData
   * @return
   */
  List<Item> getItemsByOfflineItemIds(String storeId, List<String> itemSkusForOfflineItemSearch,
    boolean setOfflineItemData);

  /**
   * Find item details by item codes
   *
   *
   * @param storeId
   * @param itemCodes
   * @return
   */
  List<ItemCodeDetailResponse> getItemDetailsByItemCodes(String storeId, Set<String> itemCodes);

  /**
   * update merchant discount price on activation and deactivation of event
   *
   * @param storeId
   * @param itemSku
   * @param activationDetail
   * @param activated
   */
  void updateMerchantDiscountPriceAndPublishEvent(String storeId, String itemSku, DiscountPrice activationDetail,
      boolean activated) throws Exception;

  /**
   * update merchant discount price on activation and deactivation of event
   *
   * @param request
   * @param storeId
   */
  void updateProductTypeOrContentChange(ProductTypeEditRequest request, String storeId) throws Exception;

  /**
   * Update Active and deactive promo bundling item level and seller level
   * @param promoBundlingActivatedDeactivatedEventModel
   */
  void activeDeactivatePromoBundling(
      PromoBundlingActivatedDeactivatedEventModel promoBundlingActivatedDeactivatedEventModel);

  /**
   * Save items changes
   * @param items
   */
  void saveItems(List<Item> items);

  /**
   * get items by store id, itemsSkus and mfd
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, Set<String> itemSkus);

  /**
   * Find markForDelete false product and itemSkus
   *
   * @param storeId
   * @param productSkus
   * @param itemSkus
   * @return
   */
  List<Item> findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(String storeId, List<String> productSkus,
      List<String> itemSkus);

  /*
   * Update Active and deactive promo bundling item pickup point level and seller level
   *
   * @param promoBundlingActivatedDeactivatedEventModel
   */
  void activeDeactivatePromoBundlingInItemPickupPoint(
      PromoBundlingActivatedDeactivatedEventModel promoBundlingActivatedDeactivatedEventModel);

  /**
   * Update Active and deactive promo bundling item pickup point level and seller level
   *
   * @param promoBundlingActivatedDeactivatedEventModel
   */
  void activeDeactivatePromoBundlingInItemPickupPointByPPCode(
      PromoBundlingActivatedDeactivatedEventModel promoBundlingActivatedDeactivatedEventModel);

  /**
   * Process adjustment product change listener and update discount price
   *
   * @param adjustmentProductChange
   *
   */
  void processAdjustmentProductChangeEventAtL5Level(AdjustmentProductChange adjustmentProductChange);

  /**
   * update ItemPickupPoint If MerchantSku Changed
   *
   * @param storeId
   * @param newItem
   * @param updatedPickupPointsList
   * @return
   */
  List<ItemPickupPoint> updateItemPickupPointDeliveryFalse(String storeId, Item newItem,
    List<ItemPickupPoint> updatedPickupPointsList);


  /**
   * update ItemPickupPoint If MerchantSku Changed
   *
   * @param storeId
   * @param existingMerchantSku
   * @param newItem
   * @param itemPickupPointWithDeliveryTrue
   * @return
   */
  List<ItemPickupPoint> updateItemPickupPointIfMerchantSkuChanged(String storeId,
    String existingMerchantSku, Item newItem, ItemPickupPoint itemPickupPointWithDeliveryTrue);

  /**
   * validate price for delivery true item
   *
   * @param storeId
   * @param itemPickupPoint
   * @param updateOfflineItemPriceRequest
   * @return
   * @throws Exception
   */
  boolean validatePriceForDeliveryTrueItemPickupPoint(String storeId, ItemPickupPoint itemPickupPoint,
      UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest);

  /**
   * fetch item map by itemSku
   * @param storeId
   * @param itemSkus
   * @return
   */
  Map<String, Item> fetchItemMapByItemSkus(String storeId, Set<String> itemSkus);

  /**
   * @param productSkus Set of productSku
   * @param storeId storeId
   * @param page pageNumber
   * @param size items per page
   * @return Paginated ItemBasicL4Response
   */
  Page<ItemBasicL4Response> getL4ItemListByProductSkuLite(Set<String> productSkus, String storeId, Integer page,
      Integer size);

  /**
   * Get L4 Item Summary response for a given set of Product Sku and sort the paginated response
   * by ItemSku
   *
   * @param storeId
   * @param productSkus
   * @return
   * @throws Exception
   */
  Page<ItemLevel4ListingResponse> getL4ItemListByProductSku(Set<String> productSkus, String storeId, Integer page, Integer size)
    throws Exception;

  /**
   * Get L5 Item List response for a given set of Product Sku from DB
   *
   * @param storeId
   * @param productSkus
   * @param fetchProductData
   * @param pickupPointCodes
   * @param promoTypes
   * @param fetchB2bData
   * @param fetchCategoryData
   * @param fetchViewConfigByChannel
   * @param excludeDistributionPickupPoint
   * @return
   * @throws Exception
   */
  List<ItemLevel5Response> getL5ItemListByProductSku(String storeId, List<String> productSkus, boolean fetchProductData,
      List<String> pickupPointCodes, List<String> promoTypes, boolean fetchB2bData, boolean fetchCategoryData,
      String fetchViewConfigByChannel, boolean excludeDistributionPickupPoint) throws Exception;

  /**
   * Find all items by itemCode and pickup point code
   *
   * @param storeId
   * @param itemCode
   * @param pickupPointCode
   * @param fetchAll
   * @return
   */
  List<ItemPriceResponse> getAllItemSkuByItemCodeAndPickupPointCode(String storeId, String itemCode,
      String pickupPointCode, boolean fetchAll);

  /**
   * get all items mapped to pristineId
   * get only items which are buyable, discoverable
   * @param storeId
   * @param pristineId
   * @param pickupPointCode
   * @return
   */
  List<ItemPriceResponse> getAllItemSkuByPristineIdAndPickupPointCode(String storeId, String pristineId,
      String pickupPointCode);

  /**
   * get itemSku by productsku and itemName keyword
   * @param storeId
   * @param keyword
   * @return
   */
  Set<String> getItemSkuByItemNameKeyword(String storeId, String productSku, String keyword);

  /**
   * Find count of cnc items by storeId and productSku
   *
   * @param storeId
   * @param productSku
   * @param cncActivated
   * @return
   */
  Long findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(String storeId, String productSku,
      boolean cncActivated);

  /**
   *
   * @param storeId
   * @param productSku
   * @param cncActivated
   * @return
   */
  Long findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivatedReadFromPrimary(String storeId, String productSku,
      boolean cncActivated);


  /**
   * get itemSku by productsku and itemName keyword
   *
   * @param storeId
   * @param merchantCode
   * @param itemCodes
   * @param keyword
   * @return
   */
  Set<String> getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(String storeId, String merchantCode,
      Set<String> itemCodes, String keyword);

  /**
   * Publish update to solr event
   *
   * @param product
   * @param itemList
   */
  void publishUpdateToSolrEvent(Product product, List<Item> itemList);

  /**
   * Update Dimension by itemcode
   * @param itemCode
   * @param length
   * @param width
   * @param weight
   * @param height
   * @param eanUpcCodes
   */
  void updateItemDimensionsAndUpcCode(String itemCode, double length, double width, double weight, double height,
      List<String> eanUpcCodes);

  /**
   * get one item by storeId, merchantCode, merchantSku and markForDelete
   *
   * @param storeId
   * @param merchantCode
   * @param merchantSku
   * @return
   */
  Item getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(String storeId,
      String merchantCode, String merchantSku);

  /**
   * get one item by storeId, merchantCode, productSkus, merchantSku and markForDelete
   *
   * @param storeId
   * @param merchantCode
   * @param merchantSku
   * @return
   */
  Item getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(String storeId,
      String merchantCode, List<String> productSkus, String merchantSku);

  /**
   * get items bu itemSku and markForDelete = false
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<Item> getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(String storeId, Set<String> itemSkus);

  /**
   * get basic item details
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return
   */
  BasicItemDTO getbasicItemDetails(String storeId, String itemSku, String pickupPointCode);

  /**
   * Get Items on the basic of item code
   * @param storeId non null store id
   * @param itemCodes distinct items codes
   * @return List of items on basis of store id and item codes
   */
  List<Item> findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(String storeId,
    Set<String> itemCodes);

  /**
   * get Item details by adding the L5 data with cnc filter
   *
   * @param storeId     must not be null
   * @param productSkus must not be empty
   * @param l5IdList
   * @param cncActivated       must not be null
   * @param page        must not be null
   * @param pageSize    must not be null
   * @return
   * @throws Exception
   */
  ItemAndItemPickupPointVo getItemAndPickupPointDetailsForCnc(String storeId, List<String> productSkus,
    List<String> l5IdList, Boolean cncActivated, Integer page, Integer pageSize);

  /**
   * get all the item details by adding the L5 data without pagination
   *
   * @param storeId
   * @param productSkus
   * @param l5Idlist
   * @return
   */
  ItemAndItemPickupPointVo getAllItemAndPickupPointDetailsWithoutPagination(String storeId, List<String> productSkus,
      List<String> l5Idlist);

  /**
   *
   * @param masterSkuMappingEventModel
   */
  void updateMasterSku(MasterSkuMappingEventModel masterSkuMappingEventModel);

  /**
   *
   * @param storeId
   * @param bundleRecipeRequestPairList
   * @param readFromPrimary
   */
  void addRecipeToOtherSisterProductForSharedProduct(String storeId,
      List<Pair<String, BundleRecipeRequest>> bundleRecipeRequestPairList, boolean readFromPrimary);


  /**
   * @param storeId
   * @param productSku
   * @param markForDelete
   * @return
   */
  List<Item> findItemsByStoreIdAndProductSkuAndMarkForDeleteReadFromPrimary(String storeId, String productSku,
      boolean markForDelete);

  /**
   * get shared product bundle recipe by item codes
   * @param storeId
   * @param itemCodes
   * @return
   */
  List<SharedProductBundleRecipeResponse> getBundleRecipeForSharedItems(String storeId, Set<String> itemCodes);

  /**
   * update recipe for shared products
   * @param storeId
   * @param bundleItems
   */
  void updateRecipeForSharedProducts(String storeId, List<Item> bundleItems) throws Exception;

  /**
   *
   * @param items
   * @return
   */
  Map<String, Boolean> getProductCodeAndSharedProductMap(Collection<Item> items);

  /**
   * @param storeId
   * @param productSku
   * @param cncActivated
   * @return
   */
  boolean existsRecordForStoreIdAndProductSkuAndCncActivated(String storeId, String productSku, boolean cncActivated);

  /**
   * @param storeId
   * @param upcCode
   * @param merchantCodes
   * @return
   */

  List<Item> fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(
      String storeId, String upcCode, Set<String> merchantCodes);

  /**
   * @param storeId
   * @param merchantCode
   * @param upcCodes
   * @return
   */
  List<UpcStatusResponse> fetchUpcCodeStatus(String storeId, String merchantCode, Set<String> upcCodes);
}
