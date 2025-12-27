package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.domain.event.model.ExternalSearchReindexToSolrEventModel;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;

public interface ProductAndItemSolrIndexerService {

  /**
   * @param item
   * @throws Exception
   */
  void applyItem(Item item) throws Exception;

  /**
   *
   * @param items
   * @throws Exception
   */
  void applyItems(List<Item> items) throws Exception;

  /**
   *
   * update in solr with product-code
   *
   * @param storeId
   * @param productCode
   */
  void updateInSolrByProductCode(String storeId, String productCode);

  /**
   * @param productDomainEventModel
   * @param productAndTotalScoreMap
   * @throws Exception
   */
  void applyMasterDataChanges(ProductDomainEventModel productDomainEventModel,
      Map<String, Double> productAndTotalScoreMap) throws Exception;

  /**
   *
   * @param masterDataDetailWithProducts
   * @param l3AtomicUpdateEnabled
   * @throws Exception
   */
  void applyMasterDataDetailWithProductAndItems(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProducts, boolean l3AtomicUpdateEnabled)
      throws Exception;

  /**
   *
   * @param product
   * @param l3AtomicUpdateEnabled
   * @throws Exception
   */
  void applyProduct(Product product, boolean l3AtomicUpdateEnabled) throws Exception;

  /**
   * @param productAndItems
   * @param l3AtomicUpdateEnabled
   * @throws Exception
   */
  void applyProductAndItems(ProductAndItemsVO productAndItems, boolean l3AtomicUpdateEnabled) throws Exception;

  /**
   * @param storeId
   * @param merchantCode
   * @throws Exception
   */
  void deleteOfflineItemByMerchantCode(String storeId, String merchantCode) throws Exception;

  /**
   * Index merchant promo discount atomically
   *
   * @param item
   * @param skuStateChange
   */
  void indexMerchantPromoDiscountItem(Item item, boolean skuStateChange) throws Exception;


  /**
   * Index merchant promo discount atomically
   *
   * @param item
   * @param activated
   * @throws Exception
   */
  void indexMerchantPromoDiscountItemPickupPoint(ItemPickupPoint item, boolean activated) throws Exception;


  /**
   * @param storeId
   * @param productSku
   * @param isSuspended
   * @param archiveItems
   * @param productArchivedFlag
   * @param right
   * @param merchantCode
   */
  void updateItemsInSolr(String storeId, String productSku, boolean isSuspended, Map<String, Boolean> archiveItems, boolean productArchivedFlag,
      List<Item> right, String merchantCode);

  /**
   * Update linked partner account for item sku
   *
   * @param storeId           store identifier
   * @param itemSku           item sku which has been successfully copied
   * @param linkedPartnerCode merchant to which SKU is copied
   */
  void updateItemSyncStatusForFulfillmentByBlibli(String storeId, String itemSku, String linkedPartnerCode) throws Exception;

  /**
   * Do atomic updates on solr on archive action
   *
   * @param item
   * @throws Exception
   */
  void updateSolrOnToggleArchiveItemAction(Item item);

  /**
   * Delete items from solr after post live product rejection
   *
   * @param itemSkuList
   */
  void deleteItemsFromSolrAfterPostLiveRejection(List<String> itemSkuList);

  /**
   * delete l4 by productSku
   *
   * @param productSku
   */
  void deleteSolrDocumentByProductSkuInL4Solr(String productSku);

  /**
   * Delete products from solr by product sku list after post live rejection
   *
   * @param productSkuList
   */
  void  deleteProductsFromSolrAfterPostLiveRejection(Set<String> productSkuList);

  /**
   * Do atomic updates on solr on sync / unsync action
   *
   * @param productAndItemsVO
   */
  void updateSolrOnSyncUnsyncAction(ProductAndItemsVO productAndItemsVO);

  /**
   * Update solr on price change
   *
   * @param items
   * @throws Exception
   */
  void updateSolrOnPriceChange(List<Item> items);

  /**
   * Update solr on promo bundling flag change
   *
   * @param itemList
   * @param promoBundling
   */
  void updateSolrOnPromoBundlingFlagChange(List<Item> itemList, boolean promoBundling);


  /**
   * Update solr on promo bundling flag change
   *
   * @param productSkuMap
   * @param promoBundling
   * @param fieldName
   * @param productSkuAndMerchantCodeMap
   */
  void updateSolrOnPromoFlagChangeByItemSkus(Map<String, List<String>> productSkuMap, boolean promoBundling,
      String fieldName, Map<String, String> productSkuAndMerchantCodeMap);

  /**
   * Update solr on master catalog changes
   *
   * @param product
   * @param items
   * @throws Exception
   */
  void updateSolrOnMasterCatalogChanges(Product product, List<Item> items);

  /**
   * Update solr on sales catalog changes
   *
   * @param product
   * @param items
   * @throws Exception
   */
  void updateSolrOnSalesCatalogChanges(Product product, List<Item> items);

  /**
   * Update solr on sales catalog changes for list of l3s
   *
   * @param products
   * @param items
   * @throws Exception
   */
  void updateSolrOnSalesCatalogChangesForProductList(List<Product> products, List<Item> items);

  /**
   * Update solr on item view config changes
   *
   * @param items
   */
  void updateSolrOnItemViewConfigChanges(List<Item> items);

  /**
   * update itemViewConfig in solr using itemPickupPoint
   * @param itemPickupPoints
   */
  void updateSolrOnItemViewConfigChangesByItemPickupPoint(List<ItemPickupPoint> itemPickupPoints);


  /**
   * Update solr on content change
   *
   * @param items
   */
  void updateSolrOnContentChange(List<Item> items);

  /**
   * Update markForDelete in l3 collection
   * @param product
   */
  void takeDownL3(Product product);

  /**
   * @param product
   * @param itemList
   * @param productInStock
   * @throws Exception
   */
  void reindexProductToL3Collection(Product product, List<Item> itemList, boolean productInStock);


  /**
   * Update wholesale price activated flag in solr
   *
   * @param itemSku
   * @param wholesalePriceActivated
   */
  void updateWholesalePriceActivatedFlag(String itemSku, Boolean wholesalePriceActivated);

  /**
   * Update wholesale price activated flag and price in solr
   *
   * @param item
   * @param wholesalePriceActivated
   */
  void updateSolrOnPriceAndWholesalePriceFlagChange(Item item, Boolean wholesalePriceActivated);

  /**
   * @param productSku
   * @throws Exception
   */
  ProductSolr getProductSolrByProductSku(String productSku) throws Exception;

  /**
   * Update solr on pristine change
   *
   * @param item
   */
  void updateSolrOnPristineChanges(List<Item> item);

  /**
   * Get pickup point codes by product sku
   *
   * @param storeId
   * @param productSku
   * @return
   */
  ProductPickupPointListResponse getPickupPointCodesByProductSku(String storeId, String productSku)
      throws Exception;

  /**
   * atomic update for listing
   * @param product
   * @param items
   * @throws Exception
   */
  void productListingAtomicUpdate(Product product, List<Item> items) throws Exception;

  /**
   * atomic update for pickupPointCodes
   * @param item
   */
  void pickupPointCodesAtomicUpdate(Item item);

  /**
   * atomic update for pickupPointCodes in L3 collection
   * @param items
   * @return
   */
  Product pickupPointCodesUpdateAndSolrPublish(List<Item> items, boolean isDifferentLocation);

  /**
   * offline item price atomic update
   * @param itemSku
   * @param offlineItems
   */
  void offlineItemPriceAtomicUpdate(String itemSku, List<OfflineItem> offlineItems);

  /**
   * Update cnc flag and offline price
   *
   * @param storeId
   * @param items
   */
  void reindexOfflineItemAndCncActivatedFlag(String storeId, List<Item> items) throws Exception;
  /**
   * l3 product reindex
   * @param productList
   * @param productSkuList
   * @throws Exception
   */
  void applyPendingProductReindex(List<Product> productList, List<String>productSkuList) throws Exception;

  /**
   * atomic update for productType
   *
   * @param productType
   * @param items
   */
  void updateSolrOnProductTypeChange(List<Item> items, ProductType productType);

  /**
   * update pickuppoint cnc flag and variant count
   *
   * @param productAndItemsVO
   * @param itemPickupPoints
   */
  void updatePickUpPointAndVariantCountAndCncActivation(ProductAndItemsVO productAndItemsVO,
      List<ItemPickupPoint> itemPickupPoints);

  /**
   *
   * @param products
   */
  void updateProductDetailsInSolr(List<Product> products);

  /**
   *
   * update product and item details in solr using event
   *
   * @param product
   * @param items
   */
  void updateProductAndItemDetailsInSolr(Product product, List<Item> items, boolean needToOverrideL4DetailsFromL5);

  /**
   * update pickupPointCodes in L3 solr
   *
   * @param productSku
   * @param pickupPointCodes
   * @param isFbbActivated
   * @param merchantCode
   */
  void updateDistinctPickupPointCodesAndL5Count(String productSku, Set<String> pickupPointCodes, int l5Count,
    boolean isFbbActivated, String merchantCode);

  /**
   *
   * @param productSku
   * @param merchantCode
   * @param fieldsAndValues
   */
  void eventBasedAtomicUpdateToSolr(String productSku, String merchantCode,
      Map<String, Object> fieldsAndValues);

  /**
   * delete l3 and l4 solr documents
   *
   * @param productDomainEventModel
   */
  void deleteL3AndL4Documents(ProductDomainEventModel productDomainEventModel);

  /**
   * reindex on external search
   *
   * @param externalSearchReindexToSolrEventModel
   * @throws Exception
   */
  void reindexOnExternalSearch(ExternalSearchReindexToSolrEventModel externalSearchReindexToSolrEventModel) throws Exception;
}
