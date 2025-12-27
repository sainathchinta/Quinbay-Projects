package com.gdn.mta.bulk.feignConfig;

import java.util.List;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.ListHolderRequest;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoRequest;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoResponse;
import com.gdn.mta.bulk.models.download.BrandAuthFilterRequest;
import com.gdn.mta.bulk.models.download.responsedata.ProductImageAndVideoResponse;
import com.gdn.x.productcategorybase.CategoryErrorResponse;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.BatchVatUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.MerchantConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface PCBFeign {

  @RequestLine("POST /api/postlive-config/merchants/bulk-add?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<BulkMerchantConfigUploadResponse> bulkMerchantConfigUpload(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, MerchantConfigurationRequestList merchantConfigurationRequestList);

  @RequestLine("POST /api/postlive-config/categories/bulk-add?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<BulkCategoryConfigUploadResponse> bulkCategoryConfigUpload(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, CategoryConfigurationRequestList categoryConfigurationRequestList);

  @RequestLine("POST /api/postlive-config/getConfigDetailsByCodes?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&configType={configType}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<BulkConfigDataResponse> fetchConfigDetailsByCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("configType") String configType, AttributeCodesRequest attributeCodesRequest);

  @RequestLine(
      "POST /api/postlive-config/getCategoryConfigurationList?"
          + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryConfigurationFilterResponse> getCategoryConfigurationList(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("page") int page,
      @Param("size") int size, ConfigurationFilterRequest configurationFilterRequest);

  @RequestLine(
        "POST /api/postlive-config/getMerchantConfigurationList?"
            + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<MerchantConfigurationFilterResponse> getMerchantConfigurationList(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("page") int page,
      @Param("size") int size, ConfigurationFilterRequest configurationFilterRequest);

  @RequestLine(
      "POST /api/postlive-config/get-configuration-status?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ConfigurationStatusResponse> getconfigurationstatus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, List<ConfigurationStatusRequest> requests);

  @RequestLine(
      "GET /api/predefinedAllowedAttributeValue/filter/getBrandSuggestions?storeId={storeId}&channelId={channelId}"
          + "&clientId={clientId}&requestId={requestId}&username={username}"
          + "&businessPartnerCode={businessPartnerCode}&page={page}&size={size}"
          + "&isSearch={isSearch}&isExternal={isExternal}&value={value}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getBrandSuggestions(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("value") String value,
      @Param("businessPartnerCode") String businessPartnerCode, @Param("isSearch") boolean isSearch,
      @Param("isExternal") boolean isExternal, @Param("page") Integer page, @Param("size") Integer size);

  @RequestLine(
      "GET /api/category/getGenericTemplateCategories?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&genericTemplateEligible"
        + "={genericTemplateEligible}&ignoreB2bExclusive={ignoreB2bExclusive}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryTreeResponse> getGenericTemplateCategories(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,
    @Param("genericTemplateEligible") boolean genericTemplateEligible, @Param("ignoreB2bExclusive") boolean ignoreB2bExclusive);

  @RequestLine("GET /api/category/categoryId/{categoryId}?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryDetailAndShippingResponse> getCategoryInfoByCategoryId(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryId") String categoryId);

  @RequestLine(
      "GET /api/category/categoryCode/{categoryCode}?storeId={storeId}&channelId={channelId}" +
          "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetailByCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryCode") String categoryCode);

  @RequestLine(
      "GET /api/attribute/{attributeId}?storeId={storeId}&channelId={channelId}" +
          "&clientId={clientId}&requestId={requestId}&username={username}&sortValues={sortValues}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AttributeResponse> getAttributeDetail(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("attributeId") String attributeId,
      @Param("sortValues") boolean sortValues);

  @RequestLine(
      "POST /api/brand-wip/filter/summary?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<BrandWipResponse> getInReviewBrands(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      BrandWipSummaryRequest brandWipSummaryRequest);

  @RequestLine("GET /api/attribute/filter/name?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&name={name}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<AttributeResponse> getAttributeByNameStartingWithAndPageable(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size, @Param("name") String name);

  @RequestLine("POST /api/category/getCategoryTree?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&catalogName={catalogName}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryTreeResponse> getCategoryTree(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("catalogName") String catalogName, List<String> categoryCodes);

  @RequestLine("POST /api/product/vat-batch-update-by-sku-codes?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<BatchVatUpdateResponse> updateVatFlagBySkuCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, BatchVatUpdateRequest batchVatUpdateRequest);

  @RequestLine("GET /api/product/productCode/{productCode}?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}&inAllProducts={inAllProducts}&originalImages={originalImages}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("inAllProducts") boolean inAllProducts,
      @Param("productCode") String productCode, @Param("originalImages") boolean originalImages);

  @RequestLine("GET /api/brand/protectedBrands?storeId={storeId}&channelId={channelId}"
    + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProtectedBrandResponse> getProtectedBrandList(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username);

  @RequestLine("GET /api/brandAuthorise/{sellerCode}/valid?storeId={storeId}&channelId={channelId}"
    + "&clientId={clientId}&requestId={requestId}&username={username}&brandCode={brandCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleBooleanResponse> getBrandAuthorisation(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("brandCode") String brandCode,@Param("sellerCode") String sellerCode);

  @RequestLine("GET api/category/basicInfo/{categoryCode}?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryResponse> getBasicCategoryInfoAndCatalogInfo(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,@Param("categoryCode") String categoryCode);

  @RequestLine("GET /api/attribute/{id}/values/{value}?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AttributeResponse> getAttributeDetailById(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("id") String attributeId, @Param("value") String value);

  @RequestLine("POST /api/brandAuthorise/delete/{brandCode}?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse deleteBrandAuthorisation(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("sellerCode") String sellerCode, @Param("brandCode") String brandCode);

  @RequestLine(
    "POST api/category/getAllChildCategoriesFromC1CategoryCode?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&filterOutInactiveCn={filterOutInactiveCn}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryCodeResponse> getAllChildCategoriesFromC1CategoryCode(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("filterOutInactiveCn") boolean filterOutInactiveCn,
    CategoryCodeRequest categoryCodeRequest);

  @RequestLine("PUT /api/category/{categoryCode}/restricted-keywords?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateCategoriesWithRestrictedKeywords(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryCode") String categoryCode,
      CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList);

  @RequestLine("GET /api/category/filter/childParent/catalog/pageablecount?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&catalogId={catalogId}"
      + "&filterType={filterType}&documentFilterType={documentFilterType}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryDTO> getChildFromParentByCatalogIdWithChildCount(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      @Param("catalogId") String catalogId, @Param("filterType") String filterType,
      @Param("documentFilterType") String documentFilterType);

  @RequestLine("POST /api/category/validateCategoryForRestrictedKeywordCategoryChange?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryErrorResponse> validateCategoryForRestrictedKeywordCategoryChange(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, List<String> categoryCodes);

  @RequestLine("POST /api/brandAuthorise/getAuthorisations?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<BrandAuthFilterResponse> getAuthorisations(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      BrandAuthFilterRequest brandAuthFilterRequest);

  @RequestLine("POST /api/brandAuthorise/create?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BrandAuthCreateResponse> create(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, BrandAuthCreateRequest brandAuthCreateRequest);

  @RequestLine("POST /api/brandAuthorise/delete?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse delete(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, List<BrandAuthDeleteRequest> brandAuthDeleteRequestList);

  @RequestLine("GET /api/attribute/{id}?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&allowedAttributeValuesTrim={allowedAttributeValuesTrim}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AttributeResponse> getAttributeDetailByAttributeId(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("id") String attributeId,
      @Param("allowedAttributeValuesTrim") boolean allowedAttributeValuesTrim);

  @RequestLine(
      "POST /api/master-attribute/{attributeCode}/values?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AttributeValueResponse> addMasterAttributeValue(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("attributeCode") String attributeCode,
      MasterAttributeAddRequest masterAttributeAddRequest);

  @RequestLine(
      "GET /api/predefinedAllowedAttributeValue/filter/getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable?storeId={storeId}&channelId={channelId}"
          + "&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&attributeId={attributeId}&value={value}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("page") Integer page,
      @Param("size") Integer size, @Param("attributeId") String attributeId, @Param("value") String value);

  @RequestLine("POST /api/attribute/filter/attributecodes?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&fetchOnlyBasicAttributeDetails={fetchOnlyBasicAttributeDetails}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<AttributeResponse> getAttributeDetailByAttributeCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("fetchOnlyBasicAttributeDetails") boolean fetchOnlyBasicAttributeDetails,
      AttributeCodesRequest attributeCodesRequest);

  @RequestLine("POST /api/product/details-by-product-codes-for-bulk-download?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<MasterProductResponse> getProductDetailListByProductCodesforBulkDownload(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, List<String> productCodeList);

  @RequestLine("GET /api/brand/filter/brand-name?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&brandName={brandName}&markForDelete={markForDelete}&activeBrandsOnly={activeBrandsOnly}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BrandResponse> filterByBrandName(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("brandName") String brandName,
      @Param("markForDelete") boolean markForDelete, @Param("activeBrandsOnly") boolean activeBrandsOnly);

  @RequestLine("POST /api/allowedattributevalue/findAllowedValues?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<AllowedAttributeValueDtoResponse> getPredefinedAndDefiningAllowedAttributeValue(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,
      ListHolderRequest<AllowedAttributeValueDtoRequest> requestList);

  @RequestLine("GET /api/category/validateIsCnCategory?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&categoryCode={categoryCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryResponse> validateIsCnCategory(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryCode") String categoryCode);

  @RequestLine("GET /api/category/hierarchy/filter/category-code/{categoryCode}?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("categoryCode") String categoryCode);

  @RequestLine("POST /api/category/hierarchy/filter/category-codes?storeId={storeId}&channelId={channelId}"
      +"&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryHierarchyResponse> filterCategoryHierarchyByCategoryCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, CategoryCodeRequest categoryCodeRequest);

  @RequestLine("POST /api/product/product-basic-info-by-product-codes?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductImageAndVideoResponse> getBasicInfoProductDetailsListByProductCodes(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, List<String> productCodeList);

  @RequestLine("GET /api/attribute/{id}/values/{value}?storeId={storeId}&channelId={channelId}"
    + "&clientId={clientId}&requestId={requestId}&username={username}&ignoreCaseFetch={ignoreCaseFetch}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AttributeResponse> getAttributeDetailByIdIgnoreCase(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("id") String attributeId,
    @Param("value") String value, @Param("ignoreCaseFetch") boolean ignoreCaseFetch);

  @RequestLine("POST /api/product/item/edit-item-upccode?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse editItemUpcCode(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, List<ProductItemUpcCodeUpdateRequest> request);

  @RequestLine(
      "POST /api/product/item/filter/get-productItemBy-sku-codes?storeId={storeId}&channelId={channelId}&clientId={clientId"
          + "}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductItemResponse> getProductItemBySkuCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SkuCodesRequest request);
}