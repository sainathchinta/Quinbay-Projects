package com.gdn.partners.pbp.outbound.product.feign;

import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.ProductMasterDataUpdateRequest;
import com.gda.mta.product.dto.response.ProductMasterDataUpdateResponse;
import com.gda.mta.product.dto.response.ProductMigrationRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.notification.dto.GdnRestSimpleResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.ListHolderRequest;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.allowedvalue.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeAndKeywordListRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.DistributionInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionIdsRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodesSkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleStringListRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.MapResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductPredictionCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleStringMapResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface PCBFeign {

  @RequestLine("GET /api/postlive-config/configuration-changes?storeId={storeId}&channelId={channelId}&clientId"
                   + "={clientId}&requestId={requestId}&username={username}&fromDateInMillis={fromDateInMillis}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ConfigurationStatusResponse> getConfigurationChanges(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("fromDateInMillis") long fromDateInMillis);

  @RequestLine("POST /api/postlive-config/get-configuration-status?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ConfigurationStatusResponse> getReviewConfiguration(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, List<ConfigurationStatusRequest> requests);


  @RequestLine("POST /api/product/republishProduct?storeId={storeId}&channelId={channelId}&clientId={clientId"
                   + "}&requestId={requestId}&username={username}&operationType={operationType}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse republishProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("operationType") String operationType, List<String> productCodes);

  @RequestLine(
      "PUT /api/product/{productCode}/update-product-category?storeId={storeId}&channelId={channelId}&clientId={clientId"
          + "}&requestId={requestId}&username={username}&categoryCode={categoryCode}&updateSalesCategory={updateSalesCategory}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategorySummaryResponse> updateProductCategory(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode,
      @Param("categoryCode") String categoryCode, @Param("updateSalesCategory") boolean updateSalesCategory, @Param("b2bSeller") boolean b2bSeller);

  @RequestLine("PUT /api/predictionCategoryMapping/upsert?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse upsertPredictionCategoryMapping(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, List<PredictionCategoryMappingRequest> requestList);

  @RequestLine("POST /api/predictionCategoryMapping/getPredictionIdAndCategoryCode?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PredictionIdAndCategoryCodeResponse> getPredictionIdAndCategoryCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      PredictionIdsRequest predictionIdsRequest);

  @RequestLine("GET /api/category/categoryCode/{categoryCode}?storeId={storeId}&channelId={channelId}&clientId"
                   + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetailByCategoryCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryCode") String categoryCode);

  @RequestLine("GET /api/category/basicInfo/{categoryCode}?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryResponse> getCategoryBasicDetailByCategoryCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryCode") String categoryCode);

  @RequestLine("GET /api/attribute/filter/attributecode?storeId={storeId}&channelId={channelId}&clientId"
                   + "={clientId}&requestId={requestId}&username={username}&attributeCode={attributeCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<AttributeResponse> getAttributeDetailByAttributeCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("attributeCode") String attributeCode);

  @RequestLine(
      "GET /api/category/{categoryCode}/restricted-keywords?storeId={storeId}&channelId={channelId}&clientId"
          + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<RestrictedKeywordsMappedToCategoryResponse> getRestrictedKeywordMappedToCategory(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("categoryCode") String categoryCode);

  @RequestLine(
      "PUT /api/product/{productCode}/update-review-pending?storeId={storeId}&channelId={channelId}&clientId"
          + "={clientId}&requestId={requestId}&username={username}&reviewPending={reviewPending}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductReviewPending(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, @Param("reviewPending") boolean reviewPending);

  @RequestLine("PUT /api/product/clearProductCacheByProductCodes?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse clearProductCacheByProductCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, List<String> productCodes);

  @RequestLine("POST /api/product/clearProductCacheSync?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&productId={productId}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse clearProductCacheSyncByProductIdAndProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productId") String productId,
      @Param("productCode") String productCode);

  @RequestLine("GET /api/brand/filter/brand-name?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&brandName={brandName}&markForDelete={markForDelete}"
      + "&activeBrandsOnly={activeBrandsOnly}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BrandResponse> filterByBrandName(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("brandName") String brandName,
      @Param("markForDelete") boolean markForDelete, @Param("activeBrandsOnly") boolean activeBrandsOnly);

  @RequestLine("GET /api/category/get-wholesale-config?storeId={storeId}&channelId={channelId}&clientId"
                   + "={clientId}&requestId={requestId}&username={username}&categoryCode={categoryCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<WholesaleMappingResponse> getWholesaleConfigToCategory(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryCode") String categoryCode);

  @RequestLine("POST /api/product/details-by-product-code-list?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&inAllProducts={inAllProducts}&originalImages={originalImages}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductDetailResponse> getAllProductDetailListByProductCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, List<String> productCodeList, @Param("inAllProducts") boolean inAllProducts,
      @Param("originalImages") boolean originalImages);

  @RequestLine("GET /api/brand-wip/detail?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&brandRequestCode={brandRequestCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BrandWipResponse> getBrandWipDetail(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("brandRequestCode") String brandRequestCode);

  @RequestLine("POST /api/product/updateProductImagesName?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&skipReview={skipReview}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ActivateImageResponse> updateProductImagesName(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("skipReview") boolean skipReview, ProductActivateImageRequest request);

  @RequestLine("POST /api/product//migrate-product?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&oldProductCode={oldProductCode}&newProductCode={newProductCode}&createdMerchant={createdMerchant}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<String> migrateProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("oldProductCode") String oldProductCode, @Param("newProductCode") String newProductCode,
      @Param("createdMerchant") String createdMerchant, ProductRequest request);

  @RequestLine("POST /api/product//migrate-product?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&oldProductCode={oldProductCode}&newProductCode={newProductCode}&createdMerchant={createdMerchant}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<String> migrateProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("oldProductCode") String oldProductCode, @Param("newProductCode") String newProductCode,
      @Param("createdMerchant") String createdMerchant);

  @RequestLine("POST /api/product/update-product-content?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}&ignoreSalesCategoryPublish={ignoreSalesCategoryPublish}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductContent(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("ignoreSalesCategoryPublish") boolean ignoreSalesCategoryPublish, ProductRequest productRequest);

  @RequestLine(
      "POST /api/product/item/filter/get-productItemBy-sku-codes?storeId={storeId}&channelId={channelId}&clientId={clientId"
          + "}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductItemResponse> getProductItemBySkuCodes(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      SkuCodesRequest request);

  @RequestLine("POST /api/product/item/edit-item-upccode?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse editItemUpcCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode, List<ProductItemUpcCodeUpdateRequest> request);

  @RequestLine("POST /api/product/update-productItem-images?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<LocationPathAndCommonImage> updateProductItemImagesByProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductItemImageUpdateRequest request);

  @RequestLine("POST /api/product/updateProductAndItemImages?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}&setDgLevel={setDgLevel}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductAndItemImagesByProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("setDgLevel") boolean setDgLevel, ProductAndItemImageRequest request);

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

  @RequestLine("POST /api/product/filter/images?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}&mainImage={mainImage}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductImageResponse> filterProductImagesByProductIds(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ListHolderRequest<String> request, @Param("mainImage") boolean mainImage);

  @RequestLine("POST /api/allowedattributevalue/findAllowedValues?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<AllowedAttributeValueResponse> findAllowedAttributeValue(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ListHolderRequest<AllowedAttributeValueRequest> requestList);

  @RequestLine("GET /api/product/item/get-itemName-by-upcCode-productCode?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}&upcCode={upcCode}&productCode={productCode}&skuCode={skuCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<SingleObjectResponse<List<String>>> getItemNameByUpcCodeAndProductCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("upcCode") String upcCode,
      @Param("productCode") String productCode, @Param("skuCode") String skuCode);


  @RequestLine("POST /api/product/item/get-itemCode-by-upcCode-productCode?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<SingleObjectResponse<List<String>>> getItemCodeByUpcCodeAndProductCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, ProductItemUpcCodesSkuCodesRequest request);

  @RequestLine("GET /api/category/validateIsCnCategory?storeId={storeId}&channelId={channelId}&clientId="
      + "{clientId}&requestId={requestId}&username={username}&categoryCode={categoryCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryResponse> validateCnCategory(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("categoryCode") String categoryCode);

  @RequestLine("GET /api/product/productCode/{productCode}?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}&inAllProducts={inAllProducts}&originalImages={originalImages}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("inAllProducts") boolean inAllProducts,
      @Param("productCode") String productCode, @Param("originalImages") boolean originalImages);
  @RequestLine(
      "PUT /api/product/{productCode}/update-flags-on-need-revision-edit?storeId={storeId}&channelId={channelId}&clientId"
          + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateFlagsOnNeedCorrection(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, NeedRevisionConfigRequest request);

  @RequestLine("GET /api/product/{productCode}/fetch-images-for-scaling?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductDetailResponse> getImagesForScalingByProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("POST /api/product/item/get-product-item-ids-by-sku-codes?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SingleObjectResponse<Map<String, String>>> getProductItemIdsBySkuCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SkuCodesRequest request);

  @RequestLine(
      "PUT /api/product/{productCode}/update-and-submit-for-revision?storeId={storeId}&channelId={channelId}&clientId"
          + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateAndMarkProductForNeedCorrection(@Param("storeId") String storeId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, ProductRequest productRequest);

  @RequestLine("GET /api/category/hierarchy/filter/category-code/{id}?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("id") String id);

  @RequestLine("POST /api/product/{productCode}/delete-original-images?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse deleteOriginalImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine("POST /api/product/update?storeId={storeId}&channelId={channelId}"
    + "&clientId={clientId}&requestId={requestId}&username={username}&isMergeRequest={isMergeRequest}"
    + "&onlyVatChanged={onlyVatChanged}&resetExtractedAttributeValue={resetExtractedAttributeValue}&ignoreSalesCategoryPublish={ignoreSalesCategoryPublish}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<NewlySavedItemResponse> updateProductItem(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("isMergeRequest") boolean isMergeRequest,
      @Param("onlyVatChanged") boolean onlyVatChanged,
      @Param("resetExtractedAttributeValue") boolean resetExtractedAttributeValue,
      @Param("ignoreSalesCategoryPublish") boolean ignoreSalesCategoryPublish, ProductRequest request);

  @RequestLine("POST /api/product/updateImages?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SingleObjectResponse<Map<String, String>>> updateImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductImageEditRequest request);

  @RequestLine(
    "GET /api/brandAuthorise/{sellerCode}/valid/?storeId={storeId}&channelId" + "={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&brandCode={brandCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleBooleanResponse> getBrandAuthorisation(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("brandCode") String brandCode,
    @Param("sellerCode") String sellerCode);

  @RequestLine("GET /api/product/item/{itemId}/get-item-name?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  SingleBaseResponse<String> getProductNameByProductItemId(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("itemId") String itemId);

  @RequestLine("POST /api/category/getCategoryNames?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryNamesResponse> getCategoryNames(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      CategoryMultipleIdRequest categoryCodes);

  @RequestLine("POST /api/product/updateCommonImages?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SingleObjectResponse<Map<String, Map<String, String>>>> updateCommonImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, List<ProductImageEditRequest> requests);

  @RequestLine("POST /api/product/create-product?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&computeCommonImage={computeCommonImage}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<MapResponse<String, String>> createProduct(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("computeCommonImage") boolean computeCommonImage,
      ProductRequest request);

  @RequestLine("GET /api/predictionCategoryMapping/predictionList/{categoryCode}?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductPredictionCategoryMappingResponse> getPredictionListByCategoryCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryCode") String categoryCode);

  @RequestLine("POST /api/brandAuthorise/isTakeDown?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleBooleanResponse> takeDownProductBasedOnBrand(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductBrandValidationRequest productBrandValidationRequest);

  @RequestLine("POST /api/product/imagesByItemCodes?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&removeOriginalImages={removeOriginalImages}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemImageResponse> getProductItemImagesByItemCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("removeOriginalImages") boolean removeOriginalImages,
      SkuCodesRequest itemCodes);

  @RequestLine("GET /api/product/{productId}/productAttributeDetails?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductAndAttributeDetailResponse> getProductAttributesByProductId(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("productId") String productId);

  @RequestLine("POST /api/product/item/filter/product-codes?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&originalImages={originalImages}"
      + "&isOnlyExternal={isOnlyExternal}&active={active}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductItemDetailResponse> getProductItemByListOfProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("originalImages") boolean originalImages,
      @Param("isOnlyExternal") boolean isOnlyExternal, @Param("active") boolean active, ProductCodesRequest request);

  @RequestLine("POST /api/product/discard?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse discardProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ProductRequest request);

  @RequestLine("GET /api/category/keywords/getCategoryRestrictedKeyword?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&id={id}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordDetail(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("id") String id);

  @RequestLine(
      "GET /api/product/{productCode}/auto-fill-product-attributes?storeId={storeId}&channelId={channelId}&clientId"
          + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<AttributeHistoryResponse> autoFillProductAttribute(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine(
      "POST /api/category/keywords/getCategoryRestrictedKeywordByCategoryCodeAndIds?storeId={storeId}&channelId={channelId}"
          + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      CategoryCodeAndKeywordListRequest categoryCodeAndKeywordListRequest);

  @RequestLine("POST /api/attribute/filter/attributecodes?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&fetchOnlyBasicAttributeDetails={fetchOnlyBasicAttributeDetails}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<AttributeResponse> getAttributeDetailByAttributeCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username,
      @Param("fetchOnlyBasicAttributeDetails") boolean fetchOnlyBasicAttributeDetails,
      AttributeCodesRequest attributeCodesRequest);

  @RequestLine(
    "POST /api/product/{productCode}/update-master-data-images-upc-code?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&ignoreSalesCategoryPublish={ignoreSalesCategoryPublish}")
  @Headers({"Content-Type: application/json", "Accept: application/json, application/xml"})
  GdnRestSingleResponse<EditProductItemAndImageResponse> updateProductMasterDataAndImagesAndUpcCode(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("productCode") String productCode,
    @Param("ignoreSalesCategoryPublish") boolean ignoreSalesCategoryPublish,
    EditProductDetailRequest editProductDetailRequest);


  @RequestLine("POST /api/product/updateBrandData?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductBrandUpdateResponse> updateProductBrandData(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ProductBrandUpdateRequest productBrandUpdateRequest);

  @RequestLine("POST /api/product/get-item-codes-by-ids?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleStringMapResponse> getSkuCodesByProductItemIds(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SimpleStringListRequest simpleStringListRequest);

  @RequestLine(
      "GET /api/predefinedAllowedAttributeValue/filter/getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable"
          + "?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page"
          + "={page}&size={size}&attributeId={attributeId}&value={value}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("page") Integer page,
      @Param("size") Integer size, @Param("attributeId") String attributeId, @Param("value") String value);

  @RequestLine("POST /api/product/activate?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId="
      + "{requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse activateProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ProductRequest request);

  @RequestLine("GET /api/product?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&"
      + "username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductResponse> getProductSummary(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size);

  @RequestLine("GET /api/product/filter/name?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}"
      + "&username={username}&page={page}&size={size}&name={name}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductResponse> getProductByName(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      @Param("name") String name);

  @RequestLine("GET /api/product/filter/nameviewableactivated?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&name={name}&"
      + "viewable={viewable}&activated={activated}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductResponse> getProductByNameAndViewableAndActivated(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      @Param("name") String name, @Param("viewable") boolean viewable, @Param("activated") boolean activated);

  @Deprecated
  @RequestLine("GET /api/product/filter/productcode?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductResponse> getProductByProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      @Param("productCode") String productCode);

  @Deprecated
  @RequestLine("GET /api/product/filter/viewable?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&viewable={viewable}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductResponse> getProductByViewable(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      @Param("viewable") boolean viewable);

  @RequestLine("GET /api/product/filter/viewableactivated?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&viewable={viewable}"
      + "&activated={activated}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductResponse> getProductByViewableAndActivated(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      @Param("viewable") boolean viewable, @Param("activated") boolean activated);

  @RequestLine("GET /api/product/getProductItemAttrValueDet?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}&productId={productId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductItemResponse> getProductItemWithAttributeValues(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productId") String productId);

  @RequestLine("POST /api/product/createGeneratedBySystem?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse createNewProductWithSpecificationDetailGeneratedBySystem(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, ProductRequest request);

  @RequestLine("POST /api/product/updateSimpleMasterData?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleMasterProductUpdateResponse> updateSimpleMasterData(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SimpleMasterProductUpdateRequest request);

  @RequestLine("POST /api/product/save?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse saveProductItem(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ProductRequest request);

  @RequestLine("GET /api/catalog/filter/type?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&catalogType={catalogType}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CatalogResponse> getCatalogByType(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      @Param("catalogType") CatalogType catalogType);

  @RequestLine(
      "POST /api/productCategory/add-product-attribute-by-prd-code?storeId={storeId}&channelId={channelId}&"
          + "clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductAttributeResponse> addProductAttributesByProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, AddProductAttributesRequest request);

  @RequestLine("GET /api/productCategory/move-product-category-by-prd-code?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}&productCode={productCode}&categoryCode={categoryCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategorySummaryResponse> movePrdCategoryByPrdCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode,
      @Param("categoryCode") String categoryCode);

  @RequestLine("GET /api/product/{id}?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}"
      + "&username={username}&originalImages={originalImages}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductDetailResponse> getProductDetailWithOriginalImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("id") String productId,
      @Param("originalImages") boolean originalImages);

  @RequestLine(
      "POST /api/product/replace/images?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId="
          + "{requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<GeneratedProductImagesPathResponse> replaceProductImages(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ReplaceProductImagesRequest productImageRequest);

  @RequestLine("POST /api/product/clearProductCache?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&productId={productId}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse clearProductCache(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productId") String productId,
      @Param("productCode") String productCode);

  @RequestLine(
      "POST /api/product/update-rejected-product?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId="
          + "{requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateRejectedProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ProductRequest request);

  @RequestLine("GET /api/product/isProductImagesActivated?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ActivateImageResponse> isProductImagesActivated(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine(
      "POST /api/product/activateAndUpdateImageName?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId="
          + "{requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse activateAndUpdateImageName(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ActivateImageRequest request);

  @RequestLine(
      "POST /api/product/update-product-image?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId="
          + "{requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductImage(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ProductRequest request);

  @RequestLine("GET /api/product/count/viewable?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&viewable={viewable}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SingleObjectResponse<Long>> getProductCountByViewable(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("viewable") boolean viewable);

  @RequestLine("GET /api/product/filter/exact-productcode?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&page={page}&size={size}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductResponse> getProductByProductCodeExactMatch(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      @Param("productCode") String productCode);

  @RequestLine("GET /api/product/update-activated?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&productCode={productCode}&activated={activated}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductActivated(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, @Param("activated") boolean activated);

  @RequestLine(
      "POST /api/product/updateProductImageName?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId="
          + "{requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ActivateImageResponse> updateProductImageName(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ActivateImageRequest request);

  @RequestLine("GET /api/product/update-viewable?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&productCode={productCode}&viewable={viewable}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductViewable(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, @Param("viewable") boolean viewable);

  @RequestLine("GET /api/product/filter/itemnameorupccode?storeId={storeId}&requestId={requestId}&username={username}"
      + "&page={page}&size={size}&itemNameOrUpcCode={itemNameOrUpcCode}&viewable={viewable}&isOnlyExternal={isOnlyExternal}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductItemResponse> getProductItemByViewableAndProductItemNameOrUpcCode(
      @Param("storeId") String storeId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("page") Integer page, @Param("size") Integer size, @Param("itemNameOrUpcCode") String itemNameOrUpcCode,
      @Param("viewable") boolean viewable, @Param("isOnlyExternal") boolean isOnlyExternal);

  @RequestLine("POST /api/product/filter/itemname/upccode?storeId={storeId}&requestId={requestId}&username={username}"
      + "&page={page}&size={size}&upcCode={upcCode}&productName={productName}&finalCategoryId={finalCategoryId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductCodeResponse> getProductItemLikeNameOrUpcCode(@Param("storeId") String storeId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("page") Integer page,
      @Param("size") Integer size, @Param("upcCode") String upcCode, @Param("productName") String productName,
      List<AttributeReqModel> attributeModelList, @Param("finalCategoryId") String finalCategoryId);

  @RequestLine("GET /api/product/filter/upccode?storeId={storeId}&requestId={requestId}&username={username}"
      + "&page={page}&size={size}&upcCode={upcCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductItemResponse> getProductItemByUpcCode(
      @Param("storeId") String storeId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("page") Integer page, @Param("size") Integer size, @Param("upcCode") String upcCode);

  @RequestLine("GET /api/product/filter/exact/upccode?storeId={storeId}&requestId={requestId}&username={username}"
      + "&page={page}&size={size}&upcCode={upcCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductItemResponse> getProductItemByUpcCodeExactMatch(
      @Param("storeId") String storeId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("page") Integer page, @Param("size") Integer size, @Param("upcCode") String upcCode);

  @RequestLine("GET /api/product/filter/itemname/category?storeId={storeId}&requestId={requestId}&username={username}"
      + "&channelId={channelId}&clientId={clientId}&page={page}&size={size}&productItemName={productItemName}"
      + "&categoryId={categoryId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductItemResponse> getProductItemByProductItemNameAndCategoryId(
      @Param("storeId") String storeId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("page") Integer page,
      @Param("size") Integer size, @Param("productItemName") String productItemName,
      @Param("categoryId") String categoryId);

  @RequestLine("GET /api/attribute/{id}?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&allowedAttributeValuesTrim={allowedAttributeValuesTrim}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AttributeResponse> getAttributeDetail(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("id") String attributeId,
      @Param("allowedAttributeValuesTrim") boolean allowedAttributeValuesTrim);

  @RequestLine("GET /api/category/{id}?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryDetailResponse> getCategoryDetail(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("id") String categoryId);

  @RequestLine("POST /api/category/hierarchy/filter/category-codes?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CategoryHierarchyResponse> filterCategoryHierarchyByCategoryCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, CategoryCodeRequest categoryCodeRequest);

  @RequestLine("GET /api/category/final-parent-category-cached?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}&categoryId={categoryId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SingleObjectResponse> getFinalParentCategoryCached(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("categoryId") String categoryId);

  @RequestLine("POST /api/category/getAllChildCategoriesFromC1CategoryCode?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&filterOutInactiveCn={filterOutInactiveCn}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CategoryCodeResponse> getAllChildCategoriesFromC1CategoryCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("filterOutInactiveCn") boolean filterOutInactiveCn,
      CategoryCodeRequest request);

  @RequestLine("GET /api/brand-wip/filter/brand-name?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&brandName={brandName}&"
      + "businessPartnerCode={businessPartnerCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BrandWipResponse> findBrandWipByBrandNameAndBusinessPartnerCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("brandName") String brandName,
      @Param("businessPartnerCode") String businessPartnerCode);

  @RequestLine("GET /api/size-chart/validateSizeChartCode/{sizeChartCode}?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse validateSizeChartCode(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("sizeChartCode") String sizeChartCode);

  @RequestLine("GET /api/size-chart/get-size-chart-name?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BasicSizeChartDetailMapResponse> getSizeChartBasicDetailBySizeChartCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      List<String> sizeChartCodes);

  @RequestLine("POST /api/scheduler/update-product-migration-status?storeId={storeId}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductMigrationStatus(@Param("storeId") String storeId,
      @Param("requestId") String requestId, ProductMigrationRequest productMigrationRequest);

  @RequestLine("POST api/product/updateMasterData?storeId={storeId}&channelId={channelId}"
    + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductMasterDataUpdateResponse> updateMasterData(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, ProductMasterDataUpdateRequest productMasterDataUpdateRequest);

  @RequestLine("GET /api/distribution-info/{productCode}?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}"
      + "&needDistributionInfoResponse={needDistributionInfoResponse}&page={page}&size={size}")
  @Headers({"Content-Type: application/json"})
  GdnRestListResponse<DistributionInfoPerSkuResponse> getDistributionInfo(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode,
      @Param("needDistributionInfoResponse") boolean needDistributionInfoResponse,
      @Param("page") int page, @Param("size") int size);

  @RequestLine("POST /api/product/v2/checkOmniChannelSkuExistsOrNotBySellerCodeAndSkuList?storeId"
      + "={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username"
      + "={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ValidOmniChannelSkuResponse> checkOmniChannelSkuExistsInSeller(
      @Param("storeId") String storeId, @Param("requestId") String requestId,
      @Param("username") String username, OmniChannelSkuRequest omniChannelSkuRequest);

  @RequestLine("POST /api/distribution-info/{productCode}?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateDistributionInfo(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode,
      DistributionInfoUpdateRequest distributionInfoUpdateRequest);

  @RequestLine("GET /api/product/getProductsByBrandName?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&brandName={brandName}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductCodeResponse> getProductsByBrandName(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("brandName") String brandName,@Param("page") Integer page,
      @Param("size") Integer size);

  @RequestLine("POST /api/product/updateBrand?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductBrandUpdateResponse> updateProductBrandName(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      com.gda.mta.product.dto.ProductBrandUpdateRequest productBrandUpdateRequest);

}
