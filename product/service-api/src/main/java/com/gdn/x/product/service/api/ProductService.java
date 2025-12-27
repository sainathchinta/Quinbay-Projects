package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.x.product.domain.event.model.CompressedVideoUpdateEventModel;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.vo.BulkDownloadProductBasicInfoResponse;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.rest.web.model.request.ProductBasicMasterFieldsRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductLevel3SummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductSkuResponse;
import com.gdn.x.product.rest.web.model.request.ReelProductListingRequest;
import com.gdn.x.product.rest.web.model.response.PromoEligibilityResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import com.gdn.x.product.domain.event.model.CategoryProductSkuMappingRequest;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;
import com.gdn.x.product.domain.event.model.ProductSkuToSalesCatalogMappingRequest;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.request.CategoryBrandRequest;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionProductActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductBundleCreationRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductNameSuggestionResponse;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.product.rest.web.model.response.ReelProductDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;

public interface ProductService {

  /**
   * method for publish all products
   * @param storeId must not be blank
   */
  void publishAllProducts(String storeId);

  /**
   * method for publish products to AGP
   * @param storeId must not be blank
   * @param productSkus must not be empty
   */
  void republishProductsToAgp(String storeId, List<String> productSkus);

  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username is optional
   * @param product must not be null, product.productCode must not be blank
   * @return boolean
   * @throws Exception if failed
   */
  boolean addProduct(String storeId, String requestId, String username, Product product)
      throws Exception;

  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username is optional
   * @return boolean
   * @throws Exception if failed
   */
  AddProductAndItemsResponseVo addProductAndItems(String storeId, String requestId, String username,
      ProductItemsVo productItemsVo) throws Exception;

  void alterSalesCategorySequence(String storeId, String productSku,
      List<SalesCategorySequence> salesCategorySequenceRequests);

  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @return false if failed
   * @throws Exception if failed
   */
  boolean deleteProduct(String storeId, String productSku) throws Exception;

  Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku);

  Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalseReadFromPrimary(String storeId, String productSku);

  List<Product> getAllSimpleProduct();

  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @return product
   * @throws Exception if failed
   */
  Product getProduct(String storeId, String productSku);

  /**
   *
   * @param storeId
   * @param productSku
   * @return
   */
  Product getProductReadFromPrimary(String storeId, String productSku);

  /**
   * @param storeId
   * @param productSku
   * @return
   */
  Product getProductFromDB(String storeId, String productSku);

  /**
   * @param storeId                 must not be blank
   * @param requestId               must not be blank
   * @param username                must not be blank
   * @param productSku              must not be blank
   * @param combineOthersBundlings
   * @param off2On
   * @param needProductData
   * @param includeForceReview
   * @param isMigrateAndSyncProduct
   * @return product and items vo
   * @throws Exception if failed
   */
  ProductItemsVo getProductAndItems(String storeId, String requestId, String username, String productSku,
    boolean showDeleted, boolean combineOthersBundlings, boolean off2On, boolean needProductData,
    boolean includeForceReview, boolean isMigrateAndSyncProduct) throws Exception;

  /**
   *
   * fetch product item details by product-sku and pickup-point-code
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param productSku
   * @param pickupPointCode
   * @param showDeleted
   * @param combineOthersBundlings
   * @param off2On
   * @param needProductData
   * @param includeForceReview
   * @return
   * @throws Exception
   */
  ProductItemsVo getProductAndItemDetails(String storeId, String requestId, String username, String productSku,
      String pickupPointCode, boolean showDeleted, boolean combineOthersBundlings, boolean off2On,
      boolean needProductData, boolean includeForceReview, String fetchViewConfigByChannel) throws Exception;

  Map<String, List<ProductAndItemsVO>> getProductAndItemsAvailability(String storeId,
      String requestId, String username, List<String> productSku) throws Exception;

  /**
   * Delete Product By StoreId And ProductSkus
   *
   * @param storeId
   * @param productSkus
   * @return
   */
  List<Product> deleteProductByStoreIdAndProductSkus(String storeId, Set<String> productSkus);

  List<ProductAndItemsVO> getProductAndItemsByItemSkus(String storeId, String requestId,
      String username, Set<String> itemSkus) throws Exception;

  /**
   * Fetch the product details for active items
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param itemSkus
   * @param fullFetch
   * @param combineOthersBundlings
   * @param off2On
   * @param needProductData
   * @return
   */
  List<ProductAndItemsVO> getProductAndItemsByItemSkusForActiveItems(String storeId, String requestId, String username,
      Set<String> itemSkus, boolean fullFetch, boolean combineOthersBundlings, boolean off2On, boolean needProductData) throws Exception;

  /**
   * Fetch the product details for all(Active/suspended/rejected) items
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param itemSkus
   * @param fullFetch
   * @param combineOthersBundlings
   * @param off2On
   * @return
   */
  List<ProductAndItemsVO> getProductAndItemsByItemSkusForAllItems(String storeId, String requestId, String username,
      Set<String> itemSkus, boolean fullFetch, boolean combineOthersBundlings, boolean off2On) throws Exception;
  
  /**
   * @param storeId
   * @param itemSku
   * @param needMasterDataDetail
   * @param fetchMfdTrueItem
   * @return
   */
  ProductItemsVo getProductAndSingleItemByItemSku(String storeId, String requestId,
      String username, String itemSku, boolean needMasterDataDetail, boolean instantPickup,
      String pickupPointCode, boolean fetchMfdTrueItem) throws Exception;

  ProductAndItemsVO getProductAndSingleItemByItemSku(String storeId, String requestId, String username, String itemSku,
      boolean needMasterDataDetail, boolean quickDetail, boolean combineOthersBundlings, boolean instantPickup,
      String pickupPointCode, boolean off2On, boolean needProductData, boolean fetchMfdTrueItem)
      throws Exception;

  ProductAndItemsVO getProductDetailAndSingleItemByItemSku(String storeId, String requestId,
      String username, String itemSku, boolean needMasterDataDetail, boolean instantPickup,
      String pickupPointCode) throws Exception;

  ProductAndItemsVO getProductDetailAndSingleItemByItemSku(String storeId, String requestId, String username, String itemSku,
      boolean needMasterDataDetail, boolean quickDetail, boolean combineOthersBundlings, boolean instantPickup,
      String pickupPointCode, boolean off2On, Boolean disablePVSwitch)
      throws Exception;

  Product getProductDeletedOrUndeleted(String storeId, String productSku);

  /**
   * @param storeId
   * @param requestId
   * @param username
   * @param productSku
   * @param needMasterDataDetail
   * @param showDeleted
   * @return
   * @throws Exception
   */
  Product getProductForView(String storeId, String requestId, String username, String productSku,
      boolean needMasterDataDetail, boolean showDeleted) throws Exception;


  /**
   * @param storeId
   * @param productSkus
   * @return
   * @throws Exception
   */
  List<Product> getProducts(String storeId, Set<String> productSkus) throws Exception;

  /**
   * get a list of unique product list by merchant code.
   *
   * @param storeId must not be blank
   * @param merchantCode must not be blank
   * @return set of unique products
   */
  List<Product> getProductsByMerchantCode(String storeId, String merchantCode);

  /**
   * @param storeId
   * @param productCatentryIds
   * @return
   */
  List<Product> getProductsByProductCatentryIds(String storeId, Set<String> productCatentryIds);

  /**
   * @param storeId
   * @param productCodes
   * @return
   */
  List<Product> getProductsByProductCodes(String storeId, Set<String> productCodes);

  /**
   * Get Products with specific fields by productCodes
   * @param storeId
   * @param productCodes
   * @param fields
   * @return
   */
  List<Product> getProductsByProductCodes(String storeId, Set<String> productCodes,
      String[] fields);

  /**
   * Get Products with specific fields by productSkus
   * @param storeId
   * @param productSkus
   * @param showDeleted
   * @param fields
   * @return
   */
  List<Product> getProductsByProductSkus(String storeId, Set<String> productSkus, String[] fields,
      boolean showDeleted);

  /**
   * get products by product code
   *
   * @param storeId
   * @param productCode
   * @return
   */
  List<Product> getProductsByProductCode(String storeId, String productCode);

  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @return boolean
   * @throws Exception if fail
   */
  ProductAndItemsVO synchronizeProduct(String storeId, String productSku) throws Exception;

  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username must not be blank
   * @param productSku must not be blank
   * @param overwriteExistingMasterData can be true or false
   * @return boolean
   * @throws Exception if fail
   */
  ProductAndItemsVO unsynchronizeProduct(String storeId, String requestId, String username, String productSku,
      boolean overwriteExistingMasterData) throws Exception;

  /**
   * @param storeId must not be blank
   * @param requestId
   * @param product must not be null
   * @param isOnlyExternal
   * @return boolean
   */
  Product updateProduct(String storeId, String requestId, Product product, boolean isOnlyExternal);

  /**
   *
   * @param storeId
   * @param merchantCode
   * @param cncActivated
   * @param username
   */
  void updateCncActivatedByMerchantCodeL3(String storeId, String merchantCode, boolean cncActivated,
      String username);

  /**
   * update cncActivated by productSku and solr L3
   *
   * @param storeId
   * @param productSkuSet
   * @param cncActivated
   * @param username
   */
  void updateCncActivatedByProductSkuAndSolrL3(String storeId, Set<String> productSkuSet, boolean cncActivated,
      String username);

  /**
   * 
   * @param storeId must not be blank
   * @param productCode must not be blank
   * @param salesCatalog must not be null
   * @param replace boolean
   * @throws Exception if failed
   */
  boolean updateProductSalesCatalogByProductCode(String storeId, String productCode,
      SalesCatalog salesCatalog, boolean replace) throws Exception;

  /**
   * Update products sales category mapping when master category is changed
   *
   * @param storeId
   * @param productCode
   * @param productSalesCategoryMapping
   * @param productCategories
   * @param bopisEligible
   * @return
   */
  boolean updateProductCatalogByProductCodeOnCategoryChange(String storeId, String productCode,
      ProductSalesCategoryMapping productSalesCategoryMapping, List<ProductCategoryDomainEventModel> productCategories,
      boolean bopisEligible);
  
  /**
   * 
   * @param storeId must not be blank
   * @param productCode must not be blank
   * @param masterCatalog must not be null
   * @return boolean
   * @throws Exception if failed
   */
  boolean updateProductMasterCatalog(String storeId, String productCode, MasterCatalog masterCatalog) throws Exception;

  /**
   * 
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param username must not be blank
   * @param productSkus must not be empty
   * @param catalogCode  must not be blank
   * @param oldCategoryCode must not be blank
   * @param newCategoryCode must not be blank
   * @return
   */
  List<Product> updateProductSalesCategory(String storeId, String requestId, String username,
      List<String> productSkus, String catalogCode, String oldCategoryCode, String newCategoryCode);

  /**
   * 
   * @param storeId storeId must not be blank
   * @param brand storeId must not be blank
   * @return
   */
  Long getProductsCountByBrand(String storeId, String brand);

  /**
   * @param storeId must not be blank
   * @param productCode must not be blank
   * @param masterDataProductAttribute must not be null
   * @return
   * @throws Exception if failed
   */
  boolean addProductAttribute(String storeId, String productCode,
      MasterDataProductAttribute masterDataProductAttribute) throws Exception;
  
  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @param masterDataProductAttribute must not be null
   * @return
   * @throws Exception if failed
   */
  boolean addProductAttributeByProductSku(String storeId, String productSku,
      MasterDataProductAttribute masterDataProductAttribute) throws Exception;

  /**
   * 
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @param salesCatalog must not be null
   * @param replace boolean
   * @throws Exception if failed
   */
  boolean updateProductSalesCatalogByProductSku(String storeId, String productSku,
      SalesCatalog salesCatalog, boolean replace) throws Exception;

  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @param masterCatalog must not be null
   * @return boolean
   * @throws Exception if failed
   */
  boolean updateProductMasterCatalogByProductSku(String storeId, String productSku,
      MasterCatalog masterCatalog);

  /**
   * to update categoryCode for the given productSku
   *
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @param categoryCode must not be blank
   * @return boolean status
   * @throws Exception
   */
  boolean updateProductMasterCategory(String storeId, String productSku, String categoryCode) throws Exception;

  /**
   * to process category to productSku mapping
   *
   * @param mappingRequest must not be blank
   */
  void processCategoryToProductSkuMapping(CategoryProductSkuMappingRequest mappingRequest) throws Exception;

  /**
   * to process productSku to salesCatalog mapping
   *
   * @param mappingRequest must not be blank
   */
  void processProductSkuToSalesCatalogMapping(ProductSkuToSalesCatalogMappingRequest mappingRequest) throws Exception;


  /**
   * get product list by store id and product sku list
   *
   * @param storeId must not null
   * @param skus must not empty or null list
   * @return
   */
  List<Product> findByStoreIdAndProductSkuIn(String storeId, List<String> skus);



  /**
   * get product by store id and product sku
   *
   * @param storeId must not null
   * @param productSku must not empty or null list
   * @return
   */
  Product findByStoreIdAndProductSku(String storeId, String productSku);

  /**
   * get product list by store id and product code list where product is not marked deleted
   *
   * @param storeId must not null
   * @param productCodes list of product codes must not null or empty
   * @return
   */
  List<Product> findByStoreIdAndProductCodeInAndMarkForDeleteFalse(String storeId,
      Set<String> productCodes);

  /**
   * Find products by storeId and productCode
   *
   * @param storeId
   * @param productCode
   * @return
   */
  List<Product> findByStoreIdAndProductCode(String storeId, String productCode);


  /**
   * API to suspend and reactivate a product
   * @param storeId
   * @param productSku
   * @param username
   * @param suspendProduct
   * @param requestId
   */
  void toggleSuspensionProduct(String storeId, String productSku, String username, boolean suspendProduct,
      String requestId) throws Exception;


  /**
   * API to fetch the product by merchant code and product code
   * @param storeId
   * @param productCode
   * @param merchantCode
   */
  List<Product> findByStoreIdAndProductCodeAndMerchantCode(String storeId, String productCode, String merchantCode);

  /**
   * save product without updating to solr
   *
   * @param product
   * @param productChangeEventTypes
   * @param productPublishSourceDeletePickupPoint
   * @return
   */
  Product saveProductWithoutUpdatingSolr(Product product, List<String> productChangeEventTypes,
      String productPublishSourceDeletePickupPoint);

  /**
   * save halal config of product and capture history
   *
   * @param product
   * @param halalHistoryUpdateEventModel
   * @param storeId
   * @return
   */
  Product saveProductHalalConfigAndCaptureHistory(Product product,
      HalalHistoryUpdateEventModel halalHistoryUpdateEventModel, String storeId);

  /**
   *
   * @param productList
   * @param itemList
   */
  void checkProductAndItemsForForceReview(List<Product> productList, List<Item> itemList);

  /**
   * Update brand for unsync products
   * @param storeId
   * @param message
   */
  void updateBrandForUnsyncProducts(String storeId, ProductDomainEventModel message);

  /**
   * Update product scores on L1 update event
   *
   * @param storeId
   * @param productCode
   * @param isBackFilling
   * @param productSku
   * @param updateCategory
   * @param isCombinedEditRequest
   * @param editProductDetailDTO
   * @param productDetails
   */
  List<ProductAndItemsVO> updateProductScoreOnMasterDataChange(String storeId, String productCode,
      boolean isBackFilling, String productSku, boolean updateCategory, boolean isCombinedEditRequest,
      EditProductDetailDTO editProductDetailDTO, Product productDetails) throws Exception;

  /**
   * Generate shipping weight
   *
   * @param storeId
   * @param categoryCode
   * @param length
   * @param width
   * @param height
   * @param weight
   * @return
   */
  double generateShippingWeight(String storeId, String categoryCode, double length, double width, double height,
      double weight);

  /**
   * Update productScore by productSku
   *
   * @param storeId
   * @param productSku
   * @param productCode
   * @param requestId      requestId
   * @param userName       username
   * @param updateCategory if True set master and sales catalog
   * @param product
   */
  List<ProductAndItemsVO> generateProductScoreByProductSku(String storeId, String productSku,
    String productCode, String requestId, String userName, boolean updateCategory, Product product) throws Exception;

  /**
   * @param requestId
   * @param productCenterSummaryRequest
   * @param pageRequest
   * @return
   */
  Page<ProductCenterSummaryResponse> getProductCenterSummary(String storeId, String requestId,
      ProductCenterSummaryRequest productCenterSummaryRequest, PageRequest pageRequest) throws Exception;

  /**
   *
   * @param storeId
   * @param masterCategories
   * @return
   * @throws Exception
   */
  List<UnmappedSkuResponse> getUnmappedProductSkus(String storeId, List<String> masterCategories) throws Exception;

  /**
   * Get productDetails by productSku
   *
   * @param storeId
   * @param productSku
   * @param needBusinessPartnerData
   * @throws Exception
   */

  ProductCenterDetailResponse getProductDetailsForProductCenter(String storeId, String productSku,
      boolean needBusinessPartnerData)
      throws Exception;

  /**
   * Update sales category by productSku
   * @param storeId
   * @param productSku
   * @param request
   * @param requestId
   */
  void updateSalesCategory(String storeId, String productSku,
      SalesCategoryMappingUpdateRequest request, String requestId) throws Exception;

  void updateMigratedProductCode(String username, String requestId, String storeId, String productSku, String newProductCode, boolean rollback) throws Exception;

  /**
   * update sales category of product
   *
   * @param salesCategoryMappingUpdateRequest
   * @param product
   */
  void updateSalesCategoryOfProduct(SalesCategoryMappingUpdateRequest salesCategoryMappingUpdateRequest,
      Product product);

  /**
   * Get unique pickup point codes by productSku
   *
   * @param storeId
   * @param productSku
   * @return
   * @throws Exception
   */
  ProductPickupPointListResponse getPickupPointCodesByProductSku(String storeId, String productSku)
      throws Exception;

  /**
   * archival action on Product
   * @param storeId
   * @param product
   * @param updatedItems
   */
  void doArchivalActionOnProduct(String storeId, Product product, List<Item> updatedItems);

  /**
   * API to fetch L3 detail by product Sku
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param productSku
   * @return
   */
  ProductAndItemsVO getProductDetailsByProductSku(String storeId, String requestId, String username,
      String productSku) throws Exception;

  /**
   *
   * @param productCode
   * @return
   */
  Long getProductL3CountByProductCode(String productCode);

  /**
   *
   * @param storeId
   * @param productSkuSummaryRequest
   * @param businessPartnerCode
   * @param page
   * @param size
   * @return
   */
  Page<ProductSkuSummaryResponse> getProductSkuList(String storeId,
      ProductSkuSummaryRequest productSkuSummaryRequest, String businessPartnerCode, int page, int size);

  /**
   * Update products Off2On status by productSku
   *
   * @param storeId
   * @param productSkuActivateMap
   * @param userName
   * @param accessChannel
   * @return
   */
  List<String> updateOff2OnFlagByProductSkus(String storeId, Map<String, Boolean> productSkuActivateMap,
      String userName, String accessChannel, String clientId, String requestId, Boolean updateOff2OnHistory);

  /**
   * Get L3 summary details by ProductSummaryRequest
   *
   * @param storeId
   * @param productSummaryRequest
   * @param onlyDefaultViewConfig
   * @return
   */
  Page<ProductL3SummaryResponse> getProductL3SummaryResponse(String storeId,
      ProductSummaryRequest productSummaryRequest, PageRequest pageRequest, boolean onlyDefaultViewConfig);

  /**
   * Get product sku list by ProductSummaryRequest
   *
   * @param storeId
   * @param productSummaryRequest
   * @return
   */
  Page<ProductSkuResponse> getProductSkuListResponse(String storeId,
      ProductSummaryRequest productSummaryRequest, PageRequest pageRequest);

  /**
   * get items by product sku cached
   * @param storeId
   * @param singleVariantProductSkus
   * @return
   */
  Map<String, List<Item>> getItemByProductSkus(String storeId, List<String> singleVariantProductSkus);

  /**
   * @param storeId must not be blank
   * @param productSku must not be blank
   * @param doArchive
   * @return false if failed
   * @throws Exception if failed
   */
  EditItemResponse toggleArchiveProduct(String storeId, String username, String productSku, boolean doArchive, String source) throws Exception;

  /**
   * Get L3 name suggestion
   *
   * @param storeId
   * @param productFilter
   * @param pageRequest
   * @return
   */
  Page<ProductNameSuggestionResponse> getProductNamesByKeyword(String storeId, ProductSummaryRequest productFilter,
      PageRequest pageRequest);


  /**
   * Get promo labels
   *
   * @param productAndItemsVO
   * @param productL3Response
   * @return
   */
  List<String> getPromoLabels(ProductAndItemsVO productAndItemsVO, ProductL3Response productL3Response);

  /**
   *
   * Get the products count by category and brand
   *
   * @param storeId
   * @param categoryBrandRequest
   * @param merchantCode
   * @return
   */
  Long getProductsCountByCategory(String storeId, CategoryBrandRequest categoryBrandRequest, String merchantCode);

  /**
   * Fetch pre order status by product skus
   *
   * @param productSkus
   * @return
   */
  Map<String, PreOrder> getPreOrderStatusByProductSkus(List<String> productSkus);


  /**
   * Generate EditProductDetailDTO
   *
   * @param updateCategory boolean
   * @param productSku     must not be null
   * @param requestId      String
   * @param editChangeType
   * @return
   */
  EditProductDetailDTO generateEditProductDetailDTO(boolean updateCategory, String productSku, String requestId,
      EditChangeType editChangeType)
      throws Exception;

  /**
   * Update edited product
   *
   * @param requestId
   * @param product
   * @param itemViewConfigMap
   * @param updateCategory
   * @param isCombinedEditRequest
   * @param editProductDetailDTO
   * @param productEditRequest
   * @return EditProductDetailDTO
   */
  EditProductDetailDTO updateEditedProduct(String requestId, Product product,
      Map<String, ItemViewConfig> itemViewConfigMap, boolean updateCategory,
      boolean isCombinedEditRequest, EditProductDetailDTO editProductDetailDTO,
      ProductEditRequest productEditRequest) throws Exception;

  /**
   * Save History for productScore update
   *
   * @param currProduct must not be null
   * @param existingProductScore must not be null
   * @param updatedProductScore must not be null
   * @return
   */
  void saveHistoryForProductScoreUpdate(Product currProduct, ProductScore existingProductScore,
      ProductScore updatedProductScore);

  /**
   * Activate product and item on need correction
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param request
   * @return
   */
  ActivateNeedRevisionResponse activateProductAndItemsOnNeedCorrection(String storeId, String username, String requestId,
      NeedCorrectionProductActivationRequest request) throws Exception;

  /**
   * Set seller promo bundlings
   *  @param storeId
   * @param productItemsVoList
   */
  void setSellerPromoBundlings(String storeId, List<ProductItemsVo> productItemsVoList);


  /**
   * update distinct pickupPointCodes in product and solr
   * @param storeId
   * @param username
   * @param productSku
   * @param itemPickupPointList
   */
  void updateDistinctPickupPointCodesAndL5Count(String storeId, String username, String productSku,
      Set<ItemPickupPoint> itemPickupPointList, int l5Count);

  /**
   * Get product type response by product code
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param productCode
   * @return
   */
  ProductTypeResponse getProductTypeByProductCode(String storeId, String requestId, String username,
      String productCode);

  /**
   *Update product L3 and L5 details based on master data change
   *
   * @param productDomainEventModel
   * @param isMigration
   */
  void updateProductAndItemDetails(ProductDomainEventModel productDomainEventModel, boolean isMigration);

  /**
   * set price and item view config from item pickup point collection
   * @param storeId
   * @param productAndItemsVO
   */
  void setPriceAndItemViewConfigFromItemPickupPoint(String storeId, ProductAndItemsVO productAndItemsVO);

  /**
   * set price and item view config from item pickup point collection for l4 listing
   * @param storeId
   * @param productAndItemsVO
   */
  void setPriceAndItemViewConfigFromItemPickupPointForItemSummary(String storeId,
      ProductAndItemsVO productAndItemsVO);

  /**
   * set master data field in product
   * @param productSku
   */
  void updateMasterDataFieldsInProduct(String storeId, String productSku) throws Exception;

  /**
   * get products by store id, merchant code and suspended flag
   *
   * @param storeId
   * @param merchantCode
   * @param suspended
   * @return
   */
  List<Product> getProductsByStoreIdAndMerchantCodeAndIsSuspended(String storeId,
      String merchantCode, boolean suspended);
  /**
   * find products by productSku and markForDelete = false
   * @param storeId
   * @param skus
   * @return
   */
  List<Product> findByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId, List<String> skus);

  /**
   *
   * @param storeId
   * @param productSkusToItemMap
   * @param products
   * @param productSolrs
   */
  void setMainImageUrl(String storeId, Map<String, List<Item>> productSkusToItemMap, List<Product> products,
      List<ProductSolr> productSolrs);

  void setCategoryEligibleForSizeChartCode(String storeId, List<ProductSolr> productSolrs,
      String attributeCode, List<Product> products);

  /**
   *
   * @param editProductDetailDTO must not be null
   */
  void processFinalSaveAndPublishEventForContentEditOnly(EditProductDetailDTO editProductDetailDTO);

  /**
   * Fetch product basic details
   *
   * @param storeId
   * @param productSkuList
   * @param needSalesCategorydata
   * @return
   */
  List<ProductBasicResponse> findProductBasicDetailsByProductSku(String storeId, SimpleListStringRequest productSkuList,
      boolean needSalesCategorydata);

  /**
   * Get product details by itemSkus
   *
   * @param storeId
   * @param productSkuList
   * @return
   */
  List<ProductBasicResponse> findProductBasicDetailsByItemSkus(String storeId, SimpleListStringRequest productSkuList);

  /**
   * to make product bundle using bundle recipe
   *
   * @param storeId
   * @param itemSku
   * @param productBundleCreationRequest
   * @throws Exception
   */

  void makeProductBundleFromBundleRecipe(String storeId, String itemSku,
      ProductBundleCreationRequest productBundleCreationRequest) throws Exception;

  /**
   * @param storeId
   * @param productSkus
   * @param readFromPrimary
   * @return
   * @throws Exception
   */
  List<Product> getAllProducts(String storeId, Set<String> productSkus, boolean readFromPrimary);

  /**
   * Find product using just product code
   * @param productCode
   * @return
   */
  Product findByProductCodeAndSellerCode(String productCode, String sellerCode);

  /**
   * reconcile product variants between pcb and x-product
   * @param storeId
   * @param requestId
   * @param username
   * @param addDeleteVariantRetryRequest
   * @return
   */
  List<ItemPickupPointCodeResponse> reconcileProductVariants(String storeId, String requestId, String username,
      AddDeleteVariantRetryRequest addDeleteVariantRetryRequest) throws Exception;

  /**
   * Check for Shared Product
   *
   * @param storeId
   * @param productCode
   * @param findByProductSku
   * @param sellerCode
   * @return
   */
  SimpleBooleanResponse isSharedProduct(String storeId, String productCode, boolean findByProductSku,
    String sellerCode);

  /**
   * get itemCode and shared product or not map
   * @param storeId
   * @param productCodes
   * @return
   */
  Map<String, Boolean> getProductCodeAndSharedProductMap(String storeId, Set<String> productCodes);

  /**
   * get products by store id and sizeChartCode
   *
   * @param sizeChartCode
   * @return
   */
  Page<ProductSkuSizeChartResponse> getActiveProductsByStoreIdAndSizeChartCode(String sizeChartCode, int page,
      int size);

  void migrateProductAndL5DetailByProductSku(String storeId, ProductAndL5MigrationRequest request)
    throws JsonProcessingException;

  /**
   * get product basic info details by product skus
   *
   * @param storeId
   * @param request
   * @return
   */
  BulkDownloadProductBasicInfoResponse getProductBasicInfoByProductSkus(String storeId, ProductLevel3SummaryRequest request);

  /**
   * check if product has promoItemSkus or not
   *
   * @param storeId
   * @param businessPartnerAndProductSkuList
   * @return
   */
  PromoEligibilityResponse isPromoItemAvailable(String storeId, Map<String, Set<String>> businessPartnerAndProductSkuList);

  /**
   * update final video data
   *
   * @param compressedVideoUpdateEventModel
   */
  void updateFinalVideoData(CompressedVideoUpdateEventModel compressedVideoUpdateEventModel);

  /**
   * update master data info
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param request
   */
  void updateMasterDataInfo(String storeId, String requestId, String username,
      ProductBasicMasterFieldsRequest request) throws Exception;

 /*
   * To fetch products to map to reels based on productSku/Name, Categories and InStock
   * @param storeId
   * @param reelProductListingRequest
   * @param pageRequest
   * @return
   */
  Page<ReelProductDetailResponse> getProductDetailsByReelProductListingRequest(String storeId,
      ReelProductListingRequest reelProductListingRequest, PageRequest pageRequest);
}
