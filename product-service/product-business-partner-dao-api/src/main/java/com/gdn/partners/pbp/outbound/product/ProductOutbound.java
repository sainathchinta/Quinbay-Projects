package com.gdn.partners.pbp.outbound.product;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.ProductBrandUpdateRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;

import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.CategoryAndCnDetailDto;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionIdsRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodesSkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;

public interface ProductOutbound {

  List<OfflineItemResponseDetail> findOfflineItems(String requestId,
      String username, String merchantCode, List<String> merchantSkus) throws Exception;

  List<UpsertOfflineItemPriceResponse> upsertOfflineItemPrice(String requestId, String username,
      String merchantCode, List<OfflineItemRequest> offlineItemRequests) throws Exception;

  Page<ItemSummaryResponse> findSummaryInstantPickupByFilter(ItemSummaryRequest filter,
      Pageable pageRequest, SortOrder sort) throws Exception;
  
  GdnBaseRestResponse updateProductMasterCatalog(String requestId, String username,
      MasterCatalogRequest masterCatalogRequest, String productCode) throws Exception;
  
  GdnBaseRestResponse addProductAttribute(String requestId, String username,
      ProductAttributeRequest productAttributeRequest, String productCode) throws Exception;

  /**
   * upsert offline item to x-product
   *
   * @param requestId
   * @param username
   * @param merchantCode
   * @param upsertOfflineItemRequests
   * @return
   * @throws Exception
   */
  List<UpsertOfflineItemPriceResponse> upsertOfflineItem(String requestId, String username,
      String merchantCode, List<UpsertOfflineItemRequest> upsertOfflineItemRequests)
      throws Exception;

  /**
   *
   * @param fromDateInMillis
   * @return
   */
  List<ConfigurationStatusResponse> getReviewConfigurationChanges(long fromDateInMillis);

  /**
   * 
   * @param configurationStatusRequests
   * @return
   */
  List<ConfigurationStatusResponse> getReviewConfiguration(List<ConfigurationStatusRequest> configurationStatusRequests);

  /**
   * Republish product
   *
   * @param operationType
   * @param products
   * @return
   */
  boolean republishProduct(String operationType, List<String> products);

  /**
   * Update product category
   *
   * @param productCode
   * @param categoryCode
   * @param updateSalesCategory
   * @return
   */
  CategorySummaryResponse updateProductCategory(String productCode, String categoryCode, boolean updateSalesCategory, boolean b2bSeller);


  /**
   * updating or inserting prediction category mapping
   *
   * @param requestList
   * @return
   */
  boolean upsertPredictionCategoryMapping(List<PredictionCategoryMappingRequest> requestList);

  /**
   * get predictionId and categoryCode from predictionIdList
   *
   * @param predictionIdsRequest
   * @return
   */
  GdnRestListResponse<PredictionIdAndCategoryCodeResponse> getPredictionIdAndCategoryCode(
      PredictionIdsRequest predictionIdsRequest);

  /**
   * Get category response by category code
   *
   * @param categoryCode
   * @return
   */
  CategoryDetailResponse getCategoryDetailByCategoryCode(String categoryCode);


  /**
   * Get category basic detail
   *
   * @param categoryCode
   * @return
   */
  CategoryResponse getCategoryBasicDetailByCategoryCode(String categoryCode);

  /**
   * Get attribute response by attribute code
   *
   * @param attributeCode
   * @return
   */
  AttributeResponse getAttributeDetailByAttributeCode(String attributeCode);

  /**
   * find category restricted keyword page by categoryCode
   * @param categoryCode
   * @return
   */
  List<String> getRestrictedKeywordMappedToCategory(String categoryCode);

  /**
   * find category restricted keyword page by categoryCode with action to be done
   *
   * @param categoryCode
   * @return
   */
  List<RestrictedKeywordsMappedToCategoryResponse> getRestrictedKeywordMappedToCategoryWithAction(String categoryCode);

  /**
   * clear master product cache by product codes
   * @param productCodes
   * @return
   */
  void clearProductCacheByProductCodes(List<String> productCodes) throws Exception;

  /**
   * Clear PCB cache by productCode and Id
   *
   * @param productId
   * @param productCode
   */
  void clearProductCacheSyncByProductIdAndProductCode(String productId, String productCode);

  ActivateImageResponse updateProductImagesName(ProductActivateImageRequest request, boolean skipReview)
      throws Exception;


  /**
   * Migrate the product details
   *
   * @param oldProductCode
   * @param newProductCode
   * @param createdMerchant
   * @param request
   * @return
   */
  String migrateProduct(String oldProductCode, String newProductCode, String createdMerchant, ProductRequest request)
      throws Exception;

  /**
   * Update the product content
   *
   * @param productRequest
   * @return
   */
  void updateProductContent(ProductRequest productRequest, boolean ignoreSalesCategoryPublish) throws Exception;

  /**
   * @param skuCodesRequest
   * @throws Exception
   */
  List<ProductItemResponse> getProductItemBySkuCodes(SkuCodesRequest skuCodesRequest) throws Exception;

  /**
   * @param productCode
   * @param productItemUpcCodeUpdateRequests
   * @throws Exception
   */
  void editItemUpcCode(String productCode, List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests)
      throws Exception;

  /**
   * @param productItemImageUpdateRequest
   * @return
   * @throws Exception
   */
  List<LocationPathAndCommonImage> updateProductItemImagesByProductCode(ProductItemImageUpdateRequest productItemImageUpdateRequest)
      throws Exception;

  /**
   * Update the product and Item Image
   *
   * @param setDgLevel, productAndItemImageRequest
   * @return
   */
  void updateProductAndItemImagesByProductCode(boolean setDgLevel, ProductAndItemImageRequest productAndItemImageRequest) throws Exception;

  /**
   * @param attributeCode
   * @param value
   * @param fetchByPredefinedAttributeCode
   * @throws Exception
   */
  PredefinedAllowedAttributeValueResponse getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
      String attributeCode, String value, boolean fetchByPredefinedAttributeCode);

  /**
   * fetch allowed attribute values by attribute code and values
   *
   * @param allowedAttributeValueRequests
   * @return
   * @throws Exception
   */
  List<AllowedAttributeValueResponse> findAllowedAttributeValue(List<AllowedAttributeValueRequest> allowedAttributeValueRequests) throws Exception;

  /**
   * Fetch images based on productIds
   * @param productIds
   * @throws Exception
   */
  List<ProductImageResponse> filterProductImagesByProductIds(List<String> productIds, boolean mainImage);

  /**
   * Verify if the cagetory code is Cn code or not
   * @param categoryCode
   * @return
   *
   */
  CategoryAndCnDetailDto validCnCategory(String categoryCode);

  /**
   * @param productCode
   * @param productItemUpcCodesSkuCodesRequest
   * @return
   */
  List<String> getItemCodesByUpcCodeAndProductCode(String productCode,
      ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest);

  /**
   * @param productCode
   * @param inAllProducts
   * @param originalImages
   * @return
   */
  ProductDetailResponse getProductDetailByProductCode(String productCode, boolean inAllProducts, boolean originalImages);

  /**
   * update flags on need correction
   * @param productCode
   * @param request
   * @throws Exception
   */
  void updateFlagsOnNeedCorrection(String productCode, NeedRevisionConfigRequest request);

  /**
   * @param productCode
   * @return
   */
  ProductDetailResponse getImagesForScalingByProductCode(String productCode);


  /**
   * @param skuCodes
   *
   * @return
   */
  Map<String, String> getProductItemIdsBySkuCodes(List<String> skuCodes);

  /**
   * Refresh product details in PCB received from vendor review
   *
   * @param productRequest
   */
  void updateAndMarkProductForNeedCorrection(ProductRequest productRequest);

  /**
   * to get category hierarchy
   *
   * @param categoryCode
   * @throws
   */
  List<CategoryResponse> filterCategoryHierarchyByCategoryCode(String categoryCode);

  /**
   * Delete original images from PCB
   *
   * @param productCode
   */
  void deleteOriginalImages(String productCode);

  /**
   * Update images in PCB
   *
   * @param request
   */
  Map<String, String> updateImages(ProductImageEditRequest request);

  /**
   *  Authorisation of protected brands
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param brandCode
   * @param businessPartnerCode
   * @return SimpleBooleanResponse
   */
  SimpleBooleanResponse authoriseProtectedBrand(String storeId,String channelId, String clientId,
   String requestId, String username, String brandCode, String businessPartnerCode);

  /**
   *
   * @param productItemId
   * @return
   */
  String findProductNameByProductItemId(String productItemId);

  /**
   * get category names by category codes
   * @param categoryCodes
   * @param page
   * @param size
   * @return
   */
  CategoryNamesResponse fetchCategoryNamesResponse(CategoryMultipleIdRequest categoryCodes, int page, int size);


  /**
   * Update common images in PCB
   *
   * @param request
   */
  Map<String, Map<String, String>> updateCommonImages(List<ProductImageEditRequest> request);

  /**
   * get item images and upc code by sku code
   * @param itemCodes
   * @param removeOriginalImages
   * @return
   */
  List<ItemImageResponse> getProductItemImagesByItemCode(SkuCodesRequest itemCodes,
      boolean removeOriginalImages);

  /**
   * Get product attribute details by productId
   *
   * @param productId
   * @return
   */
  ProductAndAttributeDetailResponse getProductAttributesByProductId(String productId);

  /**
   * Get item details by productCodes
   *
   * @param request
   * @param originalImages
   * @param isOnlyExternal
   * @param active
   * @return
   */
  List<ProductItemDetailResponse> getProductItemByListOfProductCode(ProductCodesRequest request, boolean originalImages,
      boolean isOnlyExternal, boolean active);

  /**
   * Get attribute details by attributeCodes
   *
   * @param fetchOnlyBasicAttributeDetails boolean
   * @param attributeCodesRequest must not be null
   * @return
   */
  List<AttributeResponse> getAttributeDetailByAttributeCodes(boolean fetchOnlyBasicAttributeDetails,
      AttributeCodesRequest attributeCodesRequest);

  /**
   * Update product master data in PCB
   * @param storeId 10001
   * @param requestId requestId from upstream
   * @param username username of the user who is updating the product
   * @param productMasterDataEditRequest productMasterDataEditRequest
   * @param masterProductEditDTO masterProductEditDTO
   */
  void updateProductMasterDataInPCB(String storeId, String requestId, String username,
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO);

  /**
   * Fetch distribution-info for productCode
   *
   * @param productCode
   * @return
   */
  List<DistributionInfoPerSkuResponse> getDistributionInfo(String productCode);

  /**
   * Get distribution info by productCode
   *
   * @param productCode
   * @param page
   * @param size
   * @return
   */
  GdnRestListResponse<DistributionInfoPerSkuResponse> getDistributionInfoPerSkuResponse(
      String productCode, int page, int size);

  /**
   * Get omniChannelSku to ItemCode mapping
   *
   * @param sellerCode
   * @param omniChannelSkus
   * @return
   */
  Map<String, ProductL1AndL2CodeResponse> getOmniChannelSkuToItemCode(String sellerCode,
      List<String> omniChannelSkus);

  /**
   *
   * @param productCode
   * @param distributionInfoUpdateRequest
   */
  void updateDistributionInfo(String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest);

  /**
   * Update brand value of a product
   *
   * @param productBrandUpdateRequest
   */
  ProductBrandUpdateResponse updateProductBrandValue(ProductBrandUpdateRequest productBrandUpdateRequest)
      throws Exception;
}
