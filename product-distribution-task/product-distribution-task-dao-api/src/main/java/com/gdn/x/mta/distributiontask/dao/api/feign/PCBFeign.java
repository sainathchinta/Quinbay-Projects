package com.gdn.x.mta.distributiontask.dao.api.feign;

import java.util.List;

import com.gda.mta.product.dto.response.SimpleBooleanResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface PCBFeign {

  @RequestLine("GET /api/postlive-config/configuration-changes?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&fromDateInMillis={fromDateInMillis}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ConfigurationStatusResponse> getConfigurationChanges(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("fromDateInMillis") long fromDateInMillis);

  @RequestLine("POST /api/postlive-config/get-configuration-status?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ConfigurationStatusResponse> getReviewConfiguration(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, List<ConfigurationStatusRequest> requests);

  @RequestLine("POST /api/category/hierarchy/filter/category-codes?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryHierarchyResponse> filterCategoryHierarchyByCategoryCodes(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, CategoryCodeRequest categoryCodeRequest);

  @RequestLine("GET /api/category/categoryCode/{categoryCode}?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetailByCategoryCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryCode") String categoryCode);

  @RequestLine("POST /api/category/getAllChildCategoriesFromC1CategoryCode?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryCodeResponse> getAllChildCategoriesFromC1CategoryCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, CategoryCodeRequest categoryCodeRequest);

  @RequestLine("GET /api/brandAuthorise/{sellerCode}/valid?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&brandCode={brandCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleBooleanResponse> validateAuthorisedBrand(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("brandCode") String brandCode,
      @Param("sellerCode") String sellerCode);

  @RequestLine("GET /api/brand/filter/brand-name?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&brandName={brandName}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BrandResponse> filterByBrandName(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("brandName") String brandName);

  @RequestLine("GET /api/product/productBasicDetails/{productCode}?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductResponse> getProductBasicDetailByProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("POST /api/product/clearProductCacheSync?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&productId={productId}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse clearProductCacheSyncByProductIdAndProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productId") String productId,
      @Param("productCode") String productCode);

  @RequestLine(
    "GET /api/predefinedAllowedAttributeValue/filter/getPredefinedAllowedAttributeValueByAttributeCodeAndValue?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}&attributeCode={attributeCode}&value={value}"
      + "&fetchByPredefinedAttributeCode={fetchByPredefinedAttributeCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
    @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    @Param("attributeCode") String attributeCode, @Param("value") String value,
    @Param("fetchByPredefinedAttributeCode") boolean fetchByPredefinedAttributeCode);

  @RequestLine("GET /api/product/productCode/{productCode}?storeId={storeId}&channelId="
    + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}&inAllProducts={inAllProducts}&originalImages={originalImages}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCode(
    @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username, @Param("inAllProducts") boolean inAllProducts,
    @Param("productCode") String productCode, @Param("originalImages") boolean originalImages);


}
