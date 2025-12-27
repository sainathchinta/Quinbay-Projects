package com.gdn.x.productcategorybase.service;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.dto.MasterProductDataUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductDTO;
import com.gdn.x.productcategorybase.dto.ProductDetailEditDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.UpdateNeedRevisionDTO;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;

import com.gdn.x.productcategorybase.dto.request.ProductBrandDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AuditTrailListResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.apache.commons.lang3.tuple.Pair;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.dto.CategoryChangeDTO;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.GeneratedProductImagesPathDto;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ReplaceProductImagesDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;

public interface  ProductService extends GdnBaseService<Product> {
  void activateProduct(String storeId, String id) throws Exception;
  Long countByProductCode(String storeId, String productCode);

  void deactivateProduct(String storeId, String id) throws Exception;

  void deleteProductItemsByProductAttributeValues(String storeId, List<ProductAttributeValue> productAttributeValues)
      throws Exception;

  Page<Product> findByBrandLike(String storeId, String brandName, Pageable pageable);

  Page<ProductCodeResponse> findByStoreIdAndBrandName(String storeId, String brandName,
      Pageable pageable);

  Page<Product> findByCategoryId(String storeId, String categoryId, Pageable pageable);

  Page<Product> findByMarkForDelete(String storeId, boolean markForDelete, Pageable pageable);

  Page<Product> findByName(String storeId, String name, Pageable pageable);

  Page<Product> findByNameAndCreatedBy(String storeId, String name, String createdBy, Pageable pageable);

  Page<Product> findByNameAndViewableAndActivated(String storeId, String name, boolean viewable, boolean activated,
      Pageable pageable);

  Page<Product> findByNameAndViewableAndActivatedAndUpdatedBy(String storeId, String name, boolean viewable,
      boolean activated, String updatedBy, Pageable pageable);

  /**
   * @deprecated use {@link #findByProductCodeExactMatch(String, String, Pageable)}
   * for exact match on product code
   *
   * @param storeId
   * @param productCode
   * @param pageable
   * @return
   */
  @Deprecated
  Page<Product> findByProductCode(String storeId, String productCode, Pageable pageable);

  Page<Product> findByShippingWeightBiggerOrEqualThan(String storeId, Double shippingWeight, Pageable pageable);

  Page<Product> findByShippingWeightLesserOrEqualThan(String storeId, Double shippingWeight, Pageable pageable);

  Page<Product> findByStoreIdAndCategoriesCode(String storeId, List<String> categoriesCode, Pageable pageable);

  Product getProductDetailsWithoutImagesByProductCodeAndMarkForDeleteFalse(String storeId, String id) throws Exception;

  Page<Product> findByStoreIdInitProductCategories(String storeId, Pageable pageable);

  Page<Product> findByUniqueSellingCodeLike(String storeId, String uniqueSellingCode, Pageable pageable);

  @Deprecated
  Page<Product> findByViewable(String storeId, boolean viewable, Pageable pageable);

  Page<Product> findByViewableAndActivated(String storeId, boolean viewable, boolean activated, Pageable pageable);

  /**
   * Get product with Item ,Category And Item images already initialized
   * @param storeId
   * @param viewable
   * @param activated
   * @param pageable
   * @return
   */
  Page<Product> findByViewableAndActivatedWithItemsInitialized(String storeId, boolean viewable, boolean activated, Pageable pageable);

  List<Product> findByWeightBiggerOrEqualThan(String storeId, Double weight);

  Page<Product> findByWeightBiggerOrEqualThan(String storeId, Double weight, Pageable pageable);

  List<Product> findByWeightLesserOrEqualThan(String storeId, Double weight);

  Page<Product> findByWeightLesserOrEqualThan(String storeId, Double weight, Pageable pageable);

  /**
   * Get master product information based on product codes
   *
   * @param storeId
   * @param productCodes
   * @return
   * @throws Exception
   */
  List<Product> getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(String storeId, List<String> productCodes) throws Exception;

  /**
   * Get list of basic info of products based on product codes list
   *
   * @param storeId
   * @param productCodes
   * @return List<Product>
   * @throws Exception
   */
  List<BasicInfoProductResponse> getProductBasicInfoByProductCodes (String storeId, List<String> productCodes) throws Exception;

  /**
   * markForDelete product
   *
   * @param storeId
   * @param id
   * @return
   * @throws Exception
   */
  Pair<Product, Set<String>> markForDeleteProduct(String storeId, String id) throws Exception;

  /**
   * Regenerate and update product and items
   *
   * @param storeId
   * @param savedProduct
   * @param product
   * @param pristineCategory
   * @param onlyVatChanged
   * @param scoreUpdated
   * @param resetExtractedAttributeValue
   * @param combinedUpdateForEditEnabled combined update will be enabled to ignore entity save
   * @return
   * @throws Exception
   */
  Pair<Map<String, ProductDTO>, Map<ProductItem, String>> regenerateProductItem(String storeId,
    Product savedProduct, Product product, Boolean pristineCategory, boolean onlyVatChanged,
    boolean scoreUpdated, boolean isProductDetailChanged, boolean computecommonImage,
    boolean resetExtractedAttributeValue, boolean combinedUpdateForEditEnabled) throws Exception;

  void republishProductByProductCodes(String storeId, List<String> productCodes, String operationType) throws Exception;

  String saveProductWithSpecificationDetailGenaratedBySystem(Product entity) throws Exception;

  /**
   * API to sort and generate items form product entity
   *
   * @param entity : product details
   * @return
   * @throws Exception
   */
  Product sortAndGenerateProductItem(Product entity) throws Exception;

  void updateProductWithSpecificationDetailGeneratedBySystem(String storeId, Product savedProduct, Product product)
      throws Exception;
  
  void updateProductViewable(String storeId, String productCode, boolean viewable) throws Exception;

  /**
   * update review pending flag at l3
   * @param storeId storeId
   * @param productCode productCode
   * @param reviewPending boolean
   * @return product
   */
  Product updateProductReviewPending(String storeId, String productCode, boolean reviewPending) throws Exception;


  Product updateProductActivated(String storeId, String productCode, boolean activated) throws Exception;
  
  Product checkBrandChanges(Product newProduct);
  
  /**
   * API to filter product by product code exact match
   * @param storeId
   * @param productCode
   * @param pageable
   * @return
   */
  Page<Product> findByProductCodeExactMatch(String storeId, String productCode, Pageable pageable);

  /**
   * API to get product counts from viewable criteria
   *
   * @param storeId
   * @param viewable
   * @return
   */
  Long getProductCountForViewable(String storeId, boolean viewable);

  /**
   * Regenerate Product Attribute Content
   *
   * @param savedProduct
   * @param product
   * @param updateFromVendor
   * @param categoryUpdated
   */
  void regenerateProductAttributeContent(Product savedProduct, Product product, boolean updateFromVendor,
    boolean categoryUpdated);

  /**
   * Regenerate product item content
   *
   * @param savedProduct
   * @param product
   */
  void regenerateProductItemContent(Product savedProduct, Product product);

  /**
   * Regenerate product image content
   *
   * @param savedProduct
   * @param product
   */
  void regenerateProductImageContent(Product savedProduct, Product product);

  /**
   *
   * @param product
   * @param productSalesCategoryMapping
   * @param isBrandChanged
   * @throws Exception
   */
  void publishProduct(Product product,
     ProductSalesCategoryMapping productSalesCategoryMapping, boolean isBrandChanged) throws Exception;

  /**
   * update product content
   * @param product
   * @param publishProductEvent
   * @throws Exception
   */
  void updateProductContent(Product product, boolean publishProductEvent) throws Exception;

  /**
   * Save and flush product
   *
   * @param product
   * @return
   */
  Product saveAndFlush(Product product);

  /**
   * regenerate Product Categories
   *
   * @param savedProduct
   * @param product
   * @return
   */
  CategoryChangeDTO regenerateProductCategories(Product savedProduct, Product product);

  ProductPublishUpdateDTO updateProductImage(Product product) throws Exception;
  
  Long getProductCountByBrandName(String storeId, String brandName);
  
  GeneratedProductImagesPathDto replaceProductImages(String storeId, ReplaceProductImagesDTO productImagesReq) throws Exception;

  /**
   * API to validate product with promo sku
   * 
   * @param productId
   * @param storeId
   * @param isPromoSku
   * @return
   */
  Boolean validateProductPromoSku(String productId, String storeId, boolean isPromoSku);

  /**
   * API to save product without generating product items
   * @param entity - product with items already present
   * @param commonImages
   * @return
   * @throws Exception
   */
  Product saveProduct(Product entity, List<Image> commonImages, boolean computeCommonImage) throws Exception;

  /**
   * Get Active Product Codes
   * @param storeId must not null
   * @return
   */
  Page<String> getActiveProductCodes(String storeId, Pageable pageable);

  /**
   * Find product by storeId and updateDate
   * @param storeId
   * @param startDate
   * @param endDate
   * @param pageable
   * @return
   */
  Page<Product> findProductsByStoreIdAndUpdatedDateBetween(String storeId, Date startDate,
    Date endDate, Pageable pageable);

  /**
   *
   * @param storeId
   * @param startDate
   * @param endDate
   * @param pageable
   * @return
   */
  Page<Product> findProductsByStoreIdAndUpdatedDateBetweenWithInitialization(String storeId, Date startDate,
      Date endDate, Pageable pageable);

  /**
   * publish Product publish event by updated by
   * @param storeId
   * @param updatedBy
   */
  void publishProductByStoreIdAndUpdatedBy(String storeId, String updatedBy);

  /**
   * Get products by Category Id with images, items and attribute data initialized (default is lazy)
   *
   * Filter Criteria :
   * Activated : true, Viewable : true, mark_for_delete : false
   *
   * @param storeId  store identifier
   * @param category category
   * @param pageable pageable request
   *
   * @return list of products
   */
  Page<Product> findByCategoryIdWithItemsInitialized(String storeId, String category, Pageable pageable);


  /**
   * Get category hierarchy based on upcCode of item
   *
   * @param storeId
   * @param upcCode
   * @param isOnlyExternal
   * @return list of products
   */
  List<CategoryHierarchyResponse> getCategoryHierarchyByUPCCode(String storeId, String upcCode, boolean isOnlyExternal) throws Exception;

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  Product getProductByStoreIdAndProductIdAndMarkForDeleteFalseCached(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  Product getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(String storeId, String productCode);

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  Product getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  Product getProductByStoreIdAndProductIdCached(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  Product getProductByStoreIdAndProductCodeCached(String storeId, String productCode);

  /**
   *
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setCompleteProductDetailsCached(String storeId, Product product, boolean includeMarkForDelete);

  /**
   *
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setProductCategoriesWithCategoriesCached(String storeId, Product product, boolean includeMarkForDelete);

  /**
   *
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setProductAttributesWithValuesAndAttributeCached(String storeId, Product product, boolean includeMarkForDelete);

  /**
   * Get product attribute details by productId
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductAttribute> getProductAttributes(String storeId, String productId);

  /**
   *
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setProductImagesCached(String storeId, Product product, boolean includeMarkForDelete);

  /**
   *
   * @param storeId
   * @param productCode
   */
  void evictAllProductDetailCacheByProductCode(String storeId, String productCode);

  /**
   *
   * @param storeId
   * @param product
   */
  void evictAllProductDetailCache(String storeId, Product product);

  /**
   *
   * @param storeId
   * @param productCodes
   */
  void evictProductCacheByProductCodes(String storeId, List<String> productCodes);

  /**
   *
   * @param storeId
   * @param product
   */
  void evictProductCache(String storeId, Product product);

  /**
   * Clear Product cache based on Product Code and Store Id
   * @param storeId
   * @param productCode
   * @param productId
   */
  void clearProductCacheAndProductAttributesCache(String storeId, String productCode, String productId);

  /**
   * Evit product items and product items images cache
   *
   * @param storeId
   * @param product
   */
  void evictProductItemsAndProductItemImagesCache(String storeId, Product product);

  /**
   *
   * @param product
   * @param isNewProduct
   * @throws Exception
   */
  void publishProduct(Product product, boolean isNewProduct) throws Exception;

  /**
   * publish vat update event
   * @param productItems
   */
  void publishVatUpdateEvent(List<ProductItem> productItems);

  /**
   * API to delete images for deleted products.
   *
   * @param storeId
   * @param days
   * @param daySpan
   * @param batchSize
   */
  void deleteImagesForDeletedProduct(String storeId, int days, int daySpan, int batchSize);

  /**
   * API to delete images for updated products.
   *
   * @param days
   * @param batchSize
   */
  void deleteImagesForUpdatedProduct(int days, int batchSize);

  /**
   * @param storeId
   * @param simpleMasterProductUpdateDTO
   * @return
   * @throws Exception
   */
  MasterProductDataUpdateDTO updateProductAndGetDimensionChanged(
      String storeId, SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO) throws Exception;

  Product updateProductAndItemImagesByProductCode(
      ProductAndItemImageRequest productAndItemImageRequest, String storeId, boolean setDgLevel) throws Exception;

  /**
   * @param storeId
   * @param productCode
   * @param categoryCode
   * @param b2bSeller
   * @return
   * @throws Exception
   */
  Pair<Product, CategorySummaryResponse> updateProductCategoryByStoreIdAndProductCode(String storeId,
      String productCode, String categoryCode, boolean b2bSeller) throws Exception;

  /**
   * @param request
   * @param productId
   * @return
   * @throws Exception
   */
  Product updateProductDimensions(WarehouseMasterSKUEvent request, String productId) throws Exception;

  /**
   * Fetch sales category mapping for product code
   *
   * @param storeId
   * @param productCode
   * @param ignoreHalalCategories
   * @return
   */
  ProductSalesCategoryMappingResponse getProductSalesCategoryMapping(String storeId, String productCode, boolean ignoreHalalCategories);

  /**
   * Create new product by coping product
   *
   * @param oldProductCode
   * @param newProductCode
   * @param storeId
   * @param product
   * @param createdMerchant
   * @return
   */
  String copyProduct(String oldProductCode, String newProductCode, String storeId, Product product, String createdMerchant) throws Exception;

  /**
   * API to save product without generating product items
   * @param entity - product with items already present
   * @return
   * @throws Exception
   */
  Product saveProductWithoutProductPublish(Product entity) throws Exception;

  /**
   * @param productAndItemImageRequest
   * @param storeId
   * @return
   * @throws Exception
   */
  Pair<ProductPublishUpdateDTO, List<LocationPathAndCommonImage>> updateProductItemImagesByProductCode(
      ProductItemImageUpdateRequest productAndItemImageRequest, String storeId) throws Exception;

  /**
   *
   * method to delete all original source images
   * @param imageLocations
   *
   */
  void deleteImageByImageLocations(Set<String> imageLocations);

  /**
   * API to update flags on need correction activation
   *
   * @param storeId
   * @param productCode
   * @param request
   * @return
   * @throws Exception
   */
  Product updateFlagsOnNeedCorrection(String storeId, String productCode, NeedRevisionConfigRequest request)
      throws Exception;

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  Product getImagesByProductCode(String storeId, String productCode) throws Exception;

  /**
   * Update product flags and mark product for need revision
   *
   * @param product
   * @return
   */
  UpdateNeedRevisionDTO updateAndMarkForNeedRevision(Product product) throws Exception;

  /**
   * Evict cache and publish on Mark for need revision
   *
   * @param savedProduct
   * @param isBrandChanged
   * @param salesCategoryReferenceByMasterCategory
   */
  void evictCacheOnMarkForNeedRevision(Product savedProduct,
    boolean isBrandChanged, ProductSalesCategoryMapping salesCategoryReferenceByMasterCategory) throws Exception;

  /**
   * @param storeId
   * @param productImageEditRequestList
   * @param productDTO
   * @param combinedEditRequest
   * @throws Exception
   */
  Pair<ProductPublishUpdateDTO, Map<String, Map<String, String>>> updateImages(String storeId,
    boolean computeCommonImage, List<ProductImageEditRequest> productImageEditRequestList,
    ProductDTO productDTO, boolean combinedEditRequest) throws Exception;

  /**
   * get list of productCodes eligible for deletion
   * @param storeId
   * @param updatedDate
   * @param limit
   * @return
   */
  List<String> getListOfProductCodesEligibleForDeletion(String storeId, Date updatedDate, int limit);

  /**
   * update pickedForDeletion by product codes
   * @param storeId
   * @param productCodes
   * @param pickedForDeletion
   */
  void updatePickedForDeletionFlagByProductCode(String storeId, List<String> productCodes,
      boolean pickedForDeletion);

  /**
   *
   * @param storeId
   * @param product
   * @param includeMarkForDelete
   */
  void setProductAndItemImagesCached(String storeId, Product product, boolean includeMarkForDelete);

  /**
   *
   * @param product
   */
  void evictProductAndItemImageCache(Product product);

  /**
   * Evicts Product Item Cache
   * @param product product
   */
  void evictProductItemCache(Product product);

  /**
   * Evicts Product, Product Images, Product Items and Item Images Cache
   * @param storeId storedId
   * @param product product
   */
  void evictCompleteProductAndItemsCache(String storeId, Product product);

  /**
   * Evicts Cache on Simple Product Master data update
   *
   * @param storeId     storedId
   * @param productId   productId
   * @param productCode productCode
   */
  void evictCacheForSimpleMasterProductUpdate(String storeId, String productId, String productCode);

  /**
   * Regenerate and update product and items
   *
   * @param storeId
   * @param oldProduct saved product data
   * @param newProduct product data in request
   * @param resetExtractedAttributeValue boolean
   * @param combinedUpdateForEditEnabled combined update will be enabled to ignore entity save
   * @return Pair of old and new product data
   * @throws Exception
   */
  Pair<Map<String, ProductDTO>, Map<ProductItem, String>> adjustProductItem(String storeId,
    Product oldProduct, Product newProduct, boolean computecommonImage,
    boolean resetExtractedAttributeValue, boolean combinedUpdateForEditEnabled) throws Exception;

  /**
   * PCB Update for Content and Images
   *
   * @param storeId                    10001
   * @param newProduct                 product data in request
   * @param editProductDetailRequest   complete Edit Product detail request
   * @param newlySavedItemResponseList
   * @return ProductDetailEditDTO
   * @throws Exception
   */
  ProductDetailEditDTO updateProductContentAndImages(String storeId, Product newProduct, EditProductDetailRequest editProductDetailRequest,
    List<NewlySavedItemResponse> newlySavedItemResponseList) throws Exception;

  /**
   * Evicts Product Cache and Publishes Product and VAT update events
   *
   * @param storeId                       String
   * @param oldProduct                    Product
   * @param pristineCategory              Boolean
   * @param onlyVatChanged                boolean
   * @param scoreUpdated                  boolean
   * @param itemWithOldVatValueMap        Product Item and Old Vat Value Map
   * @param ignoreSalesCategoryPublish
   * @param productAndItemLevelUpdatesDTO ProductAndItemLevelUpdatesDTO
   * @param updatedFields
   * @param deletedItems
   * @throws Exception exception
   */
  void evictCacheAndPublishForProductItemUpdate(String storeId, Product oldProduct, Product newProduct,
      Boolean pristineCategory, boolean onlyVatChanged, boolean scoreUpdated,
      Map<ProductItem, String> itemWithOldVatValueMap, boolean ignoreSalesCategoryPublish,
      ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO, Set<String> updatedFields, Set<String> deletedItems) throws Exception;

  /**
   *
   * @param newlySavedItemResponseList
   * @param product
   */
  void setProductItemIdForNewlyAddedItems(List<NewlySavedItemResponse> newlySavedItemResponseList,
      Product product);

  /**
   * @param storeId
   * @param productBrandUpdateDTO
   * @return
   * @throws Exception
   */
  Pair<Product,String> updateProductBrand(String storeId, ProductBrandUpdateDTO productBrandUpdateDTO) throws Exception;

  /**
   * Fetch product by store id and product code
   * @param storeId
   * @param productCode
   * @return
   */
  Product getProductByStoreIdAndProductCode(String storeId, String productCode);

  /**
   * Insert Missing Product Attributes
   * @param commonImageBackfillingEventModel
   * @return
   */
  Pair<Product, List<ProductAttributeValue>> fetchProductAndInsertMissingProductAttributes(
      CommonImageBackfillingEventModel commonImageBackfillingEventModel);

  /**
   * fetch Product by productCode and mfd false
   * @param storeId
   * @param productCode
   * @return
   */
  Product findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode);

  /**
   * fetch Product by id and mfd false
   * @param storeId string
   * @param productId string
   * @return Product
   */
  Product findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String productId);

  /**
   * Process Compressed Video after update
   * @param productCode product code
   * @param videoDTO Video Data
   */
  void processCompressedUpdatedVideo(String productCode, VideoDTO videoDTO)
    throws JsonProcessingException;


  /**
   * delete mfd true rows from product
   *
   * @param product
   */
  void deleteMfdTrueRowsFromProduct(Product product);

  /**
   * Update Master Data and generate external history
   *
   * @param storeId
   * @param username
   * @param productMasterDataUpdateRequest
   * @param savedProduct
   */
  AuditTrailListResponse updateMasterDataAndGenerateHistory(String storeId, String username,
      ProductMasterDataUpdateRequest productMasterDataUpdateRequest, Product savedProduct)
      throws JsonProcessingException;

  /**
   * Update brand data of a product or only brand name if required
   *
   * @param storeId
   * @param productBrandDataUpdateRequest
   */
  Product updateOnlyBrandNameOfProduct(String storeId,
      ProductBrandDataUpdateRequest productBrandDataUpdateRequest);

  /**
   * Check if omni channel exists
   *
   * @param storeId
   * @param request
   * @param needUomInfo
   * @return
   */
  ValidOmniChannelSkuResponse checkOmniChannelSkuExistsInSeller(String storeId, OmniChannelSkuRequest request,
      boolean needUomInfo);
}
