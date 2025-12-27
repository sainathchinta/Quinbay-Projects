package com.gdn.mta.product.repository;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductPredictionCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleStringMapResponse;
import com.gdn.x.productcategorybase.entity.Product;

public interface ProductRepository {

  void activate(Product product) throws Exception;

  /**
   * delete product from master data
   *
   * @param product must not null
   * @throws Exception
   */
  void delete(ProductDetailResponse product) throws Exception;

  /**
   * find product detail by passing product code
   *
   * @param productCode
   * @return
   * @throws Exception
   */
  ProductDetailResponse findProductDetailByProductCode(String productCode) throws ApplicationRuntimeException;

  /**
   * find product detail by passing product code and its mark for delete status, either true or false
   *
   * @param productCode
   * @param inAllProducts
   * @return
   * @throws Exception
   */
  ProductDetailResponse findProductDetailByProductCode(String productCode, boolean inAllProducts)
      throws Exception;

  /**
   * Light Weight API to get Product Basic detials like isActive and isViewable and updatedBy
   * @param productCode
   * @return
   * @throws Exception
   */
  ProductResponse findProductBasicDetailByProductCode(String productCode) throws Exception;

  Page<Product> findByStoreId(String storeId, Pageable pageable) throws Exception;

  Page<Product> findByStoreIdAndName(String storeId, String name, Pageable pageable) throws Exception;

  Page<Product> findByStoreIdAndNameAndViewableAndActivated(String storeId, String name, boolean viewable,
      boolean activated, Pageable pageable) throws Exception;

  /**
   * @deprecated use {@link #findByStoreIdAndProductCodeExactMatch(String, String, Pageable)}
   * for exact match on product code
   *
   * @param storeId
   * @param productCode
   * @param pageable
   * @return
   * @throws Exception
   */
  @Deprecated
  Page<Product> findByStoreIdAndProductCode(String storeId, String productCode, Pageable pageable) throws Exception;

  @Deprecated
  Page<Product> findByStoreIdAndViewable(String storeId, boolean viewable, Pageable pageable) throws Exception;

  Page<Product> findByStoreIdAndViewableAndActivated(String storeId, boolean viewable, boolean activated,
      Pageable pageable) throws Exception;

  Product findOne(String id) throws Exception;

  /**
   * get product response by product Id
   *
   * @param id must not blank
   * @return product response
   * @throws Exception
   */
  ProductDetailResponse findProductResponse(String id) throws Exception;

  void save(Product product, String businessPartnerCode) throws Exception;

  void updateForMerge(Product product) throws Exception;

  /**
   * Update master product details
   * @param simpleMasterProductUpdateRequestDTO
   * @return
   * @throws Exception
   */
  SimpleMasterProductUpdateResponse updateSimpleMasterProduct(SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO)
  throws Exception;

  /**
   * Update L1 data to PCB
   *
   * @param product
   * @param isPristineCategory
   * @param onlyVatChanged
   * @param scoreUpdated
   * @param resetExtractedAttributeValue
   * @param newlyAddedItems
   * @param productItemAttributeValueRequestMap
   * @param deletedItems
   * @param ignoreSaveItemCall
   * @param ignoreSalesCategoryPublish
   * @param deletedAttributeCodes
   * @param saveProductDataResponse
   * @return
   * @throws Exception
   */
  List<NewlySavedItemResponse> update(Product product, Boolean isPristineCategory, boolean onlyVatChanged,
      boolean scoreUpdated, boolean resetExtractedAttributeValue, List<ProductItemLevel3> newlyAddedItems,
      Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap, List<String> deletedItems,
      boolean ignoreSaveItemCall, boolean ignoreSalesCategoryPublish, List<String> deletedAttributeCodes,
      ProductL3Response saveProductDataResponse, EditProductResponse editProductResponse) throws ApplicationException;

  List<ProductItemResponse> findProductItemsByProductId(String productId) throws Exception;


  /**
   * Prepare the Product Request for Updating content in PCB
   *
   * @param product                             not null
   * @param deletedItems                        list of deleted items
   * @param isPristineCategory                  boolean
   * @param scoreUpdated                        boolean
   * @param newlyAddedItems                     list of newly added L4s
   * @param productItemAttributeValueRequestMap product item attribute value map
   * @param productDetailEditDTO
   * @param deletedAttributeCodes
   * @param savedProductDataResponse
   * @return product request
   * @throws ApplicationRuntimeException
   */
  ProductRequest prepareProductRequestForPCBUpdate(Product product, List<String> deletedItems,
      Boolean isPristineCategory, boolean scoreUpdated, List<ProductItemLevel3> newlyAddedItems,
      Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap,
      ProductDetailEditDTO productDetailEditDTO, List<String> deletedAttributeCodes,
      ProductL3Response savedProductDataResponse, EditProductResponse editProductResponse) throws ApplicationRuntimeException;

  void updateViewable(String productCode, boolean viewable) throws Exception;

  ActivateImageResponse updateProductImageName(ActivateImageRequest request) throws Exception;

  /**
   * update image locations in PCB for product and Items
   * @param request
   * @return
   * @throws Exception
   */
  ActivateImageResponse updateProductImagesName(ProductActivateImageRequest request)
      throws Exception;

  void create(ProductRequest request) throws Exception;

  /**
   * create product to PCB
   *
   * @param request must not null
   * @throws Exception
   */
  Map<String, String> createProduct(ProductRequest request, boolean computeCommonImage) throws Exception;

  void updateActivated(String productCode, boolean activated) throws Exception;

  ProductDetailResponse findDetailById(String productId) throws Exception;

  /**
   * API to get product from product-code by exact match
   *
   * @param storeId
   * @param productCode
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<Product> findByStoreIdAndProductCodeExactMatch(String storeId, String productCode,
      Pageable pageable) throws Exception;


  /**
   * API to get product counts by viewable criteria
   * @param viewable
   * @return
   * @throws Exception
   */
  Integer getProductCountByViewable(boolean viewable) throws Exception;

  void updateProductContent(ProductRequest request) throws Exception;

  void updateProductImage(ProductRequest request) throws Exception;

  /**
   * get product details by given list of product codes
   *
   * @param requestId
   * @param username
   * @param productCodeList
   * @return
   * @throws Exception
   */
  GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodes(
      String requestId, String username, List<String> productCodeList) throws Exception;

  /**
   * get all product details by given list of product codes
   *
   * @param requestId
   * @param username
   * @param productCodeList
   * @return
   * @throws Exception
   */
  GdnRestListResponse<ProductDetailResponse> getAllProductDetailsByProductCodes(String requestId, String username,
      List<String> productCodeList) throws Exception;

  /**
   * <p>Activate product image and name</p>
   *
   * @param request
   * @throws Exception
   */
  void activateAndUpdateImageName(ActivateImageRequest request) throws Exception;

  /**
   * <p>Get list all location path of product images and item images</p>
   *
   * @param productCode
   * @return
   * @throws Exception
   */
  ActivateImageResponse isProductImagesActivated(String productCode) throws Exception;

  /**
   * update rejected product
   *
   * @param productRequest
   * @throws Exception
   */
  void updateRejectedProduct(ProductRequest productRequest) throws Exception;

  List<ProductItemDetailResponse> getProductItemByListOfProductCode(List<String> productCodes, Boolean isOnlyExternal,
      boolean active)
      throws Exception;

  void clearMasterProductCache(String productCode, String productId) throws Exception;

  /**
   * clear product data use whenever need cache evict synchronous
   *
   * @param productCode product code of product
   * @param productId   id of product
   */
  void clearMasterProductCacheSync(String productCode, String productId) throws Exception;

  GeneratedProductImagesPathResponse replaceProductImages(String requestId, String username,
      ReplaceProductImagesRequest productImageRequest) throws Exception;

  GdnRestSingleResponse<CategorySummaryResponse> movePrdCategoryByPrdCode(String requestId, String username,
      String productCode, String categoryCode) throws Exception;

  GdnRestListResponse<ProductAttributeResponse> addProductAttributesByProductCode(
      String requestId, String username, AddProductAttributesRequest request) throws Exception;

  /**
   * fetch category Hierarchy by category code
   *
   * @param requestId request ID
   * @param username user name
   * @param categoryCode unique category code
   * @return
   * @throws Exception throws exception if occurs
   */
  GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(String requestId,
      String username, String categoryCode) throws Exception;

  /**
   * fetch catalog details by the catalog type.
   *
   * @param catalogType catalog type
   * @param pageable page & size
   * @return
   * @throws Exception throws exception if occurs
   */
  List<CatalogResponse> getCatalogByType(CatalogType catalogType, Pageable pageable) throws Exception;

  /**
   * Update product and item image requests
   *
   * @param productAndItemImageRequest
   * @return
   */
  void updateProductAndItemImages(ProductAndItemImageRequest productAndItemImageRequest) throws Exception;

  /**
   * Get configuration
   *
   * @param requests
   * @return
   */
  List<ConfigurationStatusResponse> getConfigurationStatus(List<ConfigurationStatusRequest> requests) throws Exception;

  /**
   * Get allowed prediction list by categoryCode
   *
   * @param categoryCode
   * @return
   */
  List<ProductPredictionCategoryMappingResponse> getPredictionListByCategoryCode(String categoryCode) throws Exception;

  /**
   * Take down product based on brand
   *
   * @param productBrandValidationRequest
   * @return
   */
  boolean takeDownProductBasedOnBrand(ProductBrandValidationRequest productBrandValidationRequest) throws Exception;

  /**
   * delete product in PCB
   * @param productRequest
   * @throws Exception
   */
  void discardProduct(ProductRequest productRequest) throws Exception;

  /**
   * Get category restricted keyword detail by ID
   *
   * @param categoryRestrictedId
   * @throws Exception
   */
  CategoryRestrictedKeywordResponse getCategoryRestrictedKeywordDetail(String categoryRestrictedId) throws Exception;

  /**
   * autofill product attribute
   * @param storeId
   * @param productCode
   * @return
   */
  List<AttributeHistoryResponse> autoFillProductAttribute(String storeId, String productCode) throws Exception;

  /**
   * Get list of CategoryRestrictedKeywordResponse
   *
   * @param categoryCode
   * @param keywordIds
   * @return
   */
  List<CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordDetailList(String categoryCode,
      List<String> keywordIds) throws Exception;


  /**
   * Update Content, upc Code and all the Images in PCB for PDP edit Request
   *
   * @param productCode not null
   * @param ignoreSalesCategoryPublish boolean
   * @param editProductDetailRequest  edit request
   * @return
   */
  EditProductItemAndImageResponse updateProductMasterDataAndImagesAndUpcCode(String productCode, boolean ignoreSalesCategoryPublish,
    EditProductDetailRequest editProductDetailRequest) throws Exception;

  /**
   * Update brand data in PCB
   *
   * @param productBrandUpdateRequest
   * @return
   * @throws Exception
   */
  ProductBrandUpdateResponse updateProductBrandData(ProductBrandUpdateRequest productBrandUpdateRequest)
      throws Exception;

  /**
   * Get sku codes by productItemIds
   *
   * @param productItemIds
   * @return
   * @throws Exception
   */
  SimpleStringMapResponse getSkuCodesByProductItemIds(List<String> productItemIds)
      throws Exception;

  /**
   * get all products mapped to a brand-name
   *
   * @param brandName
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<ProductCodeResponse> getProductsByBrandName(String brandName, Pageable pageable)
      throws Exception;
}
