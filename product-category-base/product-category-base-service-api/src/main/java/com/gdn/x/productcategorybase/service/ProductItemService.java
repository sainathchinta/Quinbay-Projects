package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Map;

import com.gdn.x.productcategorybase.dto.ProductItemDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

public interface ProductItemService extends GdnBaseService<ProductItem> {

  void activateProductItem(String storeId, ProductItem productItem) throws Exception;

  Long countBySkuCode(String storeId, String skuCode);

  void deactivateProductItem(String storeId, List<ProductItem> productItems) throws Exception;

  void deactivateProductItem(String storeId, ProductItem productItem) throws Exception;

  Page<ProductItem> findByMultipleUpcCode(String storeId, List<String> upcCodes, Pageable pageable);

  Page<ProductItem> findByMultipleUpcCodeExcludeOneItem(String storeId, List<String> upcCodes, String skuCode,
      Pageable pageable);
  /**
   * to find products by list of skuCodes and return archived product also according to fetchArchived flag
   *
   * @param storeId
   * @param skuCodes
   * @param fetchArchived
   * @throws Exception
   */
  List<ProductItem> findBySkuCodes(String storeId, List<String> skuCodes, boolean fetchArchived) throws Exception;

  Page<ProductItem> findByStoreIdAndGeneratedItemName(String storeId, String generatedItemName, Pageable pageable)
      throws Exception;

  /**
   *
   * @param upcCode
   * @param isOnlyExternal
   * @param categoryIds
   * @return
   */
  List<String> getProductItemsIdsByUpcCodeAndCategoryIds(
      String upcCode, boolean isOnlyExternal, List<String> categoryIds);

  /**
   *
   * @param storeId
   * @param productItemIds
   * @param pageable
   * @return
   */
  Page<ProductItem> getProductItemsByStoreIdAndIds(
      String storeId, List<String> productItemIds, Pageable pageable);

  Page<ProductItem> findByStoreIdAndGeneratedItemNameAndCategoryId(String storeId, String generatedItemName, String categoryId, Pageable pageable)
      throws Exception;

  ProductItem findByStoreIdAndId(String storeId, String productItemId) throws Exception;

  /**
   * find by storeId and productItemId
   *
   * @param storeId
   * @param productItemId
   * @throws Exception
   */
  ProductItem findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String productItemId);

  /**
   * Find by existing omniChannelSkus
   *
   * @param storeId
   * @param sellerCode
   * @param omniChannelSkus
   * @return
   */
  List<ProductItem> findByStoreIdAndCreatedMerchantAndOmniChannelSkuInAndMarkForDeleteFalse(String storeId,
      String sellerCode, List<String> omniChannelSkus);

  ProductItem findByStoreIdAndSkuCode(String storeId, String skuCode) throws Exception;


  Page<ProductItem> findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(String storeId, boolean viewable,
      boolean isOnlyExternal, String itemNameOrUpcCode, Pageable pageable);

  /**
   * Use {@link #findByUpcCodeExactMatch(String, String, Pageable)} instead
   *
   * @param storeId
   * @param upcCode
   * @param pageable
   * @return
   */
  @Deprecated
  Page<ProductItem> findByUpcCode(String storeId, String upcCode, Pageable pageable);

  List<ProductItem> findProductItemByProductIdInitAttrValue(String storeId, String productId);

  String getSequence(String productCode);

  /**
   * Get item code sequence inside transaction block
   *
   * @param productCode
   * @return
   */
  String getSequenceTransaction(String productCode);

  boolean isUpcAvailable(String storeId, String upcCode, boolean activated) throws Exception;

  /**
   * to save list of item requests
   *
   * @param storeId
   * @param productItemFromRequest
   * @param productCode
   * @throws Exception
   */
  Product saveProductItemDetails(String storeId, List<ProductItem> productItemFromRequest, String productCode) throws Exception;

  Page<Object[]> findDuplicateProductItemsOrByUpcCode(String storeId, String upcCode,
                                                      String productName, Pageable pageable) throws Exception;

  Page<ProductItem> findByUpcCodeExactMatch(String storeId, String upcCode, Pageable
      pageable);

  List<ProductItem> findByListOfProductCode(List<String> productCodes, Boolean isOnlyExternal, boolean active);

  /**
   *
   * @param upcCode
   * @param isOnlyExternal
   * @return
   */
  Map<String, Long> getCategoryIdsWithProductCountForUPCCode(String upcCode, Boolean isOnlyExternal);

  /**
   * Fetch ProductItem entity using Product entity foreign key
   *
   * @param storeId
   * @param product
   * @param pageable
   * @return
   */
  Page<ProductItem> findListOfItemsByProduct(String storeId, Product product, Pageable pageable);

  /**
   * Fetch item details by item code
   *
   * @param storeId
   * @param itemCode
   * @return
   */
  ProductItem getProductItemByItemCode(String storeId, String itemCode);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductItem> getProductItemsByStoreIdAndProductIdCached(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductItem> getProductItemsByStoreIdAndProductId(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param productItem
   */
  void setProductCached(String storeId, ProductItem productItem);

  /**
   *
   * @param storeId
   * @param product
   * @return
   */
  List<ProductItemAttributeValue> getProductItemAttributeValuesWithAttributesCached(
      String storeId, Product product);

  /**
   *
   * @param productItem
   */
  void removeDeletedProductItemImages(ProductItem productItem);

  /**
   * @param productItem
   */
  void removeDeletedProductItemImagesWithoutFilteringMainImages(ProductItem productItem);

  /**
   * Removes Deleted Product Item Images and sets Main Image Flag
   *
   * @param productItemDTO product and Items DTO
   */
  void removeDeletedAndSetMainImageFlagForProductItemImages(ProductItemDTO productItemDTO);

  /**
   *
   * @param storeId
   * @param product
   * @return
   */
  List<ProductItemImage> getProductItemImagesCached(String storeId, Product product);

  /**
   * @param storeId
   * @param productItemUpcCodeUpdateRequests
   * @param productCode
   * @throws Exception
   */
  List<ProductItem> updateProductItemUpcCode(String storeId, List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests,
      String productCode) throws Exception;

  /**
   * @param upcCode
   * @param productCode
   * @return
   */
  List<String> getItemNameByUPCCodeAndProductCode(String upcCode, String productCode, String skuCode);

  /**
   *
   * @param storeId
   * @param skuCodeList
   * @return
   */
  Map<String, String> getProductItemIdsBySkuCode(String storeId, List<String> skuCodeList);

  /**
   * Find list of items without initializing forign mappings by sku code list and markForDelete false
   *
   * @param storeId
   * @param skuCodes
   * @return
   */
  List<ProductItem> findItemsBySkuCodesAndMarkForDeleteFalse(String storeId, List<String> skuCodes);

  /**
   * Save product item list
   *
   * @param productItem
   * @return
   */
  ProductItem saveProductItem(ProductItem productItem);

  /**
   * get product item by item code
   * @param storeId
   * @param skuCode
   * @return
   */
  ProductItem getProductItemBySkuCode(String storeId, String skuCode);

  /**
   * @param upcCodes
   * @param productCode
   * @param skuCodes
   * @return
   */
  List<String> getItemCodeByUPCCodeAndProductCode(List<String> upcCodes, String productCode, List<String> skuCodes);

  /**
   * Get product skuCodes by item ids
   *
   * @param storeId
   * @param productItemIds
   * @return
   */
  Map<String, String> getBySkuCodeByProductItemIds(String storeId, List<String> productItemIds);
}
