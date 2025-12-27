package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public interface ProductHelperService {

  /**
   * @param product must not be null
   * @param itemSku must not be blank
   * @param itemAttributeValues must not be null
   * @return product
   */
  Product addItemAttributeToProductAttribute(Product product, String itemSku,
      List<MasterDataItemAttributeValue> itemAttributeValues);

  /**
   * @param requestId
   * @param username
   * @param categoryCodes
   * @return
   */
  List<List<CategoryResponse>> constructListOfCategoriesListOfProduct(String requestId,
      String username, List<String> categoryCodes) throws Exception;

  /**
   * Construct map of category hierarchy to product category code
   *
   * @param requestId
   * @param username
   * @param categoryCodes
   * @return
   */
  Map<String, List<CategoryResponse>> constructMapOfCategoriesAndCategoryCode(String requestId,
      String username, List<String> categoryCodes);

  /**
   * @param productAvailables product from db
   * @param itemAvailableCandidate item candidate
   * @param masterDataProducts master data product and items
   * @return
   */
  Map<String, List<ProductAndItemsVO>> constructProductWithItemAndMasterData(
      List<Product> productAvailables, Map<String, List<Item>> itemAvailableCandidate,
      Map<String, MasterDataProductAndItemsVO> masterDataProducts);

  /**
   * @param itemRequestVO must not be null
   * @return true or false
   */
  boolean containDefaultChannelPrice(Item itemRequestVO);

  /**
   * @param listOfItemRequestVO must not be null
   * @return true or false
   */
  boolean containDefaultChannelPriceForListOfItemRequestVO(List<Item> listOfItemRequestVO);

  /**
   * @param product must not be null
   * @param itemSku must not be blank
   * @return product updated
   */
  Product deleteItemAttributeFromProductAttribute(Product product, String itemSku);

  String generateSpecificationDetail(Product entity);


  /**
   * If channel is null, then default value will be set
   *
   * @param itemViewConfigs must not be null
   * @param channel must not be null
   * @param isArchived
   * @return buyable status according to schedule
   * @throws Exception if fail
   */
  boolean getCurrentBuyableStatusForItem(Set<ItemViewConfig> itemViewConfigs, String channel, boolean isArchived)
      throws Exception;


  /**
   * If channel is null, then default value will be set
   *
   * @param itemViewConfigs must not be null
   * @param channel         must not be null
   * @param isArchived
   * @return discoverable status according to schedule
   * @throws Exception if fail
   */
  boolean getCurrentDiscoverableStatusForItem(Set<ItemViewConfig> itemViewConfigs, String channel, boolean isArchived)
      throws Exception;

  /**
   * Get Original Buyable Status For Item
   *
   * @param itemViewConfigs
   * @param isArchived
   * @return
   * @throws Exception
   */
  boolean getOriginalBuyableStatusForItem(Set<ItemViewConfig> itemViewConfigs, boolean isArchived)
      throws Exception;

  /**
   * Get Original Discoverable Status For Item
   *
   * @param itemViewConfigs
   * @param isArchived
   * @return
   * @throws Exception
   */
  boolean getOriginalDiscoverableStatusForItem(Set<ItemViewConfig> itemViewConfigs, boolean isArchived)
      throws Exception;

  Map<String, MasterDataItem> getMasterDataItemsByItems(String storeId, String requestId,
      String username, List<Item> items) throws Exception;

  /**
   * @param prices must not be empty
   * @param channel must not be null
   * @return sale price if current date is relevant or offer price with specified channel
   * @throws Exception if fail
   */
  Price getRelevantItemPrice(Set<Price> prices, String channel) throws Exception;

  /**
   * @param product
   * @param item
   * @return
   */
  String getSettlementType(Product product, Item item);

  /**
   * @param items must not be null
   * @param oldProductName must not be blank
   * @param newProductName must not be blank
   * @return
   */
  List<Item> modifyItemNames(List<Item> items, String oldProductName, String newProductName);

  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @param sizeOfProductAttributes starts from one
   * @param itemRequestVO must not be null
   * @return item
   */
  Item setItemDetail(String storeId, String productSku, String merchantCode,
      int sizeOfProductAttributes,
      Item itemRequestVO);

  /**
   *
   * @param storeId
   * @param productSku
   * @param merchantCode
   * @param sizeOfProductAttributes
   * @param itemVo
   * @return
   */
  ItemVo setItemDetail(String storeId, String productSku, String merchantCode,
      int sizeOfProductAttributes,
      ItemVo itemVo);

  /**
   * @param requestId must not be blank
   * @param username must not be blank
   * @param item must not be null
   * @return item with filled master data item field
   * @throws Exception if fail
   */
  Item setMasterDataItemFromMasterData(String storeId, String requestId, String username, Item item)
      throws Exception;

  /**
   * @param requestId must not be blank
   * @param username must not be blank
   * @param product must not be null
   * @return product with filled master data product field
   * @throws Exception if fail
   */
  Product setMasterDataProductFromMasterData(String storeId, String requestId, String username,
      Product product) throws Exception;

  /**
   * @param requestId must not be blank
   * @param items must not be null
   * @return list of items with filled master data item fields
   * @throws Exception
   */
  List<Item> setMultipleMasterDataItemsFromMasterData(String storeId, String requestId,
      String username, List<Item> items) throws Exception;

  /**
   * setting all the product details in product entity
   *
   * @param requestId
   * @param username
   * @param storeId
   * @param product
   * @param productDetail
   * @return
   * @throws Exception
   */
  Product setProductDetail(String requestId, String username, String storeId, Product product,
      ProductDetailResponse productDetail) throws Exception;

  /**
   * @param currItem must not be null
   * @param itemViewConfig must not be null
   * @return null if channel not found
   */
  Item updateItemViewConfigForExistingChannel(Item currItem, ItemViewConfig itemViewConfig);

  /**
   * update itemViewConfig at item pickup point
   * @param itemPickupPoint
   * @param itemViewConfig
   */
  boolean updateItemViewConfigForExistingChannelItemPickupPoint(ItemPickupPoint itemPickupPoint,
      ItemViewConfig itemViewConfig);

  /**
   * @param itemToSet
   * @param itemViewConfigs
   * @return
   */
  void updateItemViewConfigForExistingChannel(Item itemToSet, Set<ItemViewConfig> itemViewConfigs);

  /**
   * list of category List of product, those are exists in PCB
   *
   * @param requestId
   * @param username
   * @param categoryCodes
   * @return
   */
  List<List<CategoryResponse>> getCategoryResponseListByCategoryCodesForProducts(String requestId,
      String username, List<String> categoryCodes);

  void constructOfflineItem(Item item, OfflineItem offlineItem);

  /**
   * Construct offline item and item for transaction
   *  @param item
   * @param itemPickupPoint
   */
  void constructOfflineItemForTransaction(Item item, ItemPickupPoint itemPickupPoint);

  void findAndConstructOfflineItems(String storeId, List<Item> item);

  /**
   * find offline item then if exist, then it will set to item attributes
   *
   * @param storeId
   * @param items
   * @param pickupPointCode
   */
  void findAndConstructOfflineItemsByPickupPointCode(String storeId, List<Item> items, String pickupPointCode);

  void overwriteItemPriceWithOfflinePrice(Set<Price> prices, OfflineItem offlineItem);

  void overwriteItemViewConfigsForOfflineItem(Set<ItemViewConfig> itemViewConfigs);

  /**
   * Get category detail from cache
   *
   * @param requestId
   * @param username
   * @param categoryCode
   * @return
   */
  CategoryResponse getCategoryResponseByCategoryCode(String requestId, String username, String categoryCode);

  /**
   * get the items by product sku acached and removed recently updated item and override with L5 data
   * @param storeId
   * @param productSku
   * @param itemSkus
   * @return
   */
  List<Item> getCachedItemsByProductSku(String storeId, String productSku, List<String> itemSkus);


  /**
   * get the items by product sku acached and removed recently updated item and without overriding L5 data
   *
   * @param storeId
   * @param productSku
   * @param itemSkus
   * @return
   */
  List<Item> getCachedItemsByProductSkuWithoutOverridingL5Data(String storeId, String productSku, List<String> itemSkus);

  /**
   * @param pickupPointToSet
   * @param itemViewConfigs
   * @return
   */
  void updateItemPickupPointViewConfigForExistingChannel(ItemPickupPoint pickupPointToSet,
    Set<ItemViewConfig> itemViewConfigs);

  /**
   *
   * @param currentItemViewConfig must not be null
   * @param itemViewConfig must not be null
   * @return null if channel not found
   */
  ItemViewConfig updateItemPickupPointViewConfigForExistingChannel(ItemViewConfig currentItemViewConfig, ItemViewConfig itemViewConfig);
}