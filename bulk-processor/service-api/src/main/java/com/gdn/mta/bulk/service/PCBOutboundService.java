package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Map;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.download.BrandAuthFilterRequest;
import com.gdn.mta.bulk.models.download.responsedata.ProductImageAndVideoResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.CategoryErrorResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MerchantConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;

public interface PCBOutboundService {

  /**
   * bulk merchant configuration upload
   *
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param merchantConfigurationRequestList
   * @return
   */
  GdnRestListResponse<BulkMerchantConfigUploadResponse> bulkMerchantConfigUpload(String storeId, String channelId,
      String clientId, String requestId, String username,
      MerchantConfigurationRequestList merchantConfigurationRequestList);

  /**
   * bulk category configuration upload
   *
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param categoryConfigurationRequestList
   * @return
   */
  GdnRestListResponse<BulkCategoryConfigUploadResponse> bulkCategoryConfigUpload(String storeId, String channelId,
      String clientId, String requestId, String username,
      CategoryConfigurationRequestList categoryConfigurationRequestList);

  /**
   * fetch configuration detail for multiple code by configType
   *
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param configType
   * @param attributeCodesRequest
   * @return
   */
  GdnRestListResponse<BulkConfigDataResponse> fetchConfigDetailsByCodes(String storeId, String channelId,
      String clientId, String requestId, String username, String configType,
      AttributeCodesRequest attributeCodesRequest);

  /**
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param configurationFilterRequest
   * @param pageNumber
   * @param size
   * @return
   */
  GdnRestListResponse<CategoryConfigurationFilterResponse> getCategoryConfigurationList(String storeId,
      String channelId, String clientId, String requestId, String username,
      ConfigurationFilterRequest configurationFilterRequest, int pageNumber, int size);

  /**
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param configurationFilterRequest
   * @param pageNumber
   * @param size
   * @return
   */
  GdnRestListResponse<MerchantConfigurationFilterResponse> getMerchantConfigurationList(String storeId,
      String channelId, String clientId, String requestId, String username,
      ConfigurationFilterRequest configurationFilterRequest, int pageNumber, int size);

  /**
   * To fetch the product reviewConfig by merchantCode and categoryCode
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param requests
   * @return
   */
   GdnRestListResponse<ConfigurationStatusResponse> getconfigurationstatus(String storeId,
      String channelId, String clientId, String requestId, String username, List<ConfigurationStatusRequest> requests);

  /**
   * To fetch brand which are in review for Business Partner
   * @param value
   * @param businessPartnerCode
   * @param isSearch
   * @param isExternal
   * @return
   */
  List<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(String value, String businessPartnerCode,
      boolean isSearch, boolean isExternal);

  /**
   * get category details by categoryCode
   * @param categoryCode
   * @return
   */
  CategoryDetailResponse getCategoryDetailResponse(String categoryCode);

  /**
   * get attribute details by attributeId
   * @param attributeId
   * @return
   */

  AttributeResponse getAttributeDetail(String attributeId);

  /**
   * To get generic template categories
   *
   * @param genericTemplateEligible
   * @param ignoreB2bExclusive
   * @return
   */
  List<CategoryTreeResponse> getGenericTemplateCategories(boolean genericTemplateEligible,
    boolean ignoreB2bExclusive);

  /**
   * Get category detail by category code
   *
   * @param categoryCode
   * @return
   */
  CategoryResponse getBasicCategoryInfoAndCatalogInfo(String categoryCode);

  /**
   * Get category detail by category id
   *
   * @param categoryId
   * @return
   */
  CategoryDetailAndShippingResponse getCategoryInfoByCategoryId(String categoryId);

  /**
   * Get in review brands from PCB
   *
   * @param brandWipSummaryRequest
   * @return
   */
  List<BrandWipResponse> getInReviewBrands(BrandWipSummaryRequest brandWipSummaryRequest);

  /**
   * Get attribute by name starting with and pageable
   *
   * @param attributeName
   * @return
   */
  List<AttributeResponse> getAttributeByNameStartingWithAndPageable(String attributeName);

  /**
   *
   * @param categoryCode
   * @return
   */
  CategoryTreeResponse getCategoryTree(String categoryCode);

  /**
   *
   * @param businessPartnerCode
   * @return
   */
  List<BatchVatUpdateResponse> batchVatFlagUpdate(String username, String businessPartnerCode,
      Map<String, Boolean> batchVatUpdateRequest);

  /**
   * @param productCode
   * @param inAllProducts
   * @param originalImages
   * @return
   */
  ProductDetailResponse getProductDetailByProductCode(String productCode, boolean inAllProducts, boolean originalImages);

  /**
   * To fetch List of protected brand
   *
   * @param storeId
   * @return ProtectedBrandListResponse
   */
  List<ProtectedBrandResponse> getProtectedBrandList(String storeId);

  /**
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param sellerCode
   * @param brandCode
   * @return Boolean Response of Brand Authorisation
   */
  SimpleBooleanResponse getBrandAuthorisation(String storeId, String channelId, String clientId,
    String requestId, String username, String sellerCode, String brandCode);

  /**
   * get attribute response by id and code
   *
   * @param storeId
   * @param attributeId
   * @param value
   * @return
   */
  AttributeResponse getAttributeDetailById(String storeId, String attributeId, String value);

  /**
   * delete brand authorisation
   * @param storeId
   * @param sellerCode
   * @param brandCode
   */
  void deleteBrandAuthorisation(String storeId, String sellerCode, String brandCode);


  /**
   * Fetch Child Categories From C1
   * @param requestId
   * @param categoryCodeRequest
   * @param filterOutInactiveCn
   */

  List<String> getAllChildCategoriesFromC1CategoryCode(String requestId,
    CategoryCodeRequest categoryCodeRequest, boolean filterOutInactiveCn);

  /**
   *
   * @param storeId
   * @param categoryCode
   * @param categoryKeywordUpdateRequestList
   * @return
   */
  String updateCategoriesWithRestrictedKeywords(String storeId, String categoryCode,
      CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList);
  /**
   * @param storeId
   * @param page
   * @param size
   * @param filterType
   * @param documentFilterType
   * @param username
   * @return
   */
  List<CategoryDTO> getChildFromParentByCatalogIdWithChildCount(String storeId, int page, int size,
      String filterType, String documentFilterType, String username);

  /**
   *
   * @param storeId
   * @param categoryCodes
   * @return
   */
  List<CategoryErrorResponse> validateDestinationCategory(String storeId, List<String> categoryCodes);

  /**
   *
   * @param storeId
   * @param page
   * @param size
   * @param brandAuthFilterRequest
   * @return
   */
  GdnRestListResponse<BrandAuthFilterResponse> getAuthorisations(String storeId, int page, int size, BrandAuthFilterRequest brandAuthFilterRequest);

  /**
   * @param storeId
   * @param brandAuthCreateRequest
   * @return
   */
  String createBulkBrandAuthorisation(String storeId, BrandAuthCreateRequest brandAuthCreateRequest);

  /**
   * @param storeId
   * @param brandAuthDeleteRequestList
   * @return
   */
  String deleteBulkBrandAuthorisation(String storeId, List<BrandAuthDeleteRequest> brandAuthDeleteRequestList);

  /**
   * Filters the category hierarchy based on the provided list of category codes.
   *
   * @param categoryCodes A list of category codes to filter the category hierarchy.
   *                      Each category code represents a cn category in the hierarchy.
   * @return A list of {@link CategoryHierarchyResponse} objects representing the category hierarchy.
   *         The list contains details of categories that match the provided category codes.
   */
  List<CategoryHierarchyResponse> filterCategoryHierarchyByCategoryCodes(List<String> categoryCodes);

  /**
   * Retrieves basic product information including images and videos from PCB (Product Category Base) system.
   *
   * @param productCodesList List of product codes for which information needs to be retrieved
   * @return List of {@link ProductImageAndVideoResponse} containing product images and video information
   *         for the requested product codes
   */
  List<ProductImageAndVideoResponse> getBasicInfoFromPCB(List<String> productCodesList);

  /**
   * Fetch brand attribute with ignore case
   * @param storeId String store id
   * @param attributeId String attribute id
   * @param value String value
   * @return Attribute
   */
  AttributeResponse getAttributeDetailByIdIgnoreCase(String storeId, String attributeId, String value);

  /**
   * Send edit Upc request to PCB
   * @param productCode String product code
   * @param productItemUpcCodeUpdateRequests contains sku code and upc code
   * @return boolean request for update request
   */
  boolean editItemUpcCode(String productCode, List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests);

  /**
   * Send get product item request from PCB
   * @param skuCodesRequest contains sku code
   * @return ProductItemResponse
   */
  List<ProductItemResponse> getProductItemBySkuCodes(SkuCodesRequest skuCodesRequest);
}

