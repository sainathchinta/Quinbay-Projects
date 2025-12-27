package com.gdn.mta.bulk.feignConfig;

import java.util.List;

import com.gdn.mta.bulk.dto.product.ProductAndBrandResponse;
import org.springframework.web.bind.annotation.RequestBody;

import com.gda.mta.product.dto.BrandAndCategoryItemSummaryRequest;
import com.gda.mta.product.dto.BrandUpdateRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gda.mta.product.dto.BusinessPartnerCodeResponseList;
import com.gda.mta.product.dto.CreateProductRequest;
import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemPriceStockQuickUpdateResponse;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gda.mta.product.dto.NeedRevisionProductsRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductCodesResponse;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.SuspensionProductRequestList;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gda.mta.product.dto.response.DeleteInProgressL5Response;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.InProgressProductsByPickupPointCodeResponse;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductCodeAndNameDetails;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.models.ProductMasterDataEditRequest;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.ListRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface PBPFeign {

  @RequestLine("GET /api/product-level3/filter/activeBrandByCategoryId?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&categoryId={categoryId}&clearCache={clearCache}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PredefinedAllowedAttributeValueResponse> getActiveBrandsByCategoryId(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("categoryId") String categoryId,
      @Param("clearCache") boolean clearCache);

  @RequestLine("POST /api/offline-item/delete/bulk?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<DeleteOfflineItemResponse> bulkDeleteOfflineItem(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("merchantCode") String merchantCode,
      ListRequest<DeleteOfflineItemRequest> request);

  @RequestLine("POST /api/product-level3/archive?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&doArchive={doArchive}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ItemBulkArchiveResponse> bulkArchiveProductSkus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("doArchive") boolean doArchive, SimpleListStringRequest request);

  @RequestLine(
      "POST /api/product-level3/product/update-return?"
          + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&clientHost={clientHost}"
          + "&isOnlyExternal={isOnlyExternal}&hasOrder={hasOrder}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductLevel3Response> updateAndReturn(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("clientHost") String clientHost,
      @Param("isOnlyExternal") Boolean isOnlyExternal, @Param("hasOrder") boolean hasOrder,
      @RequestBody ProductLevel3Request product);

  @RequestLine(
      "PUT /api/product/{productCode}/update-product-category?"
          + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&categoryCode={categoryCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductCategory(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, @Param("categoryCode") String categoryCode);

  @RequestLine(
      "GET /api/product/{merchantCode}/fetch-in-progress-products?"
          + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<InProgressProductResponse> fetchInProgressProductsByMerchantCode(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("merchantCode") String merchantCode, @Param("page") int page,
    @Param("size") int size);

  @RequestLine(
      "POST /api/product-level3/{productSku}/updateProductItemViewConfig?"
          + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductItemViewConfig(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku,
      ProductLevel3ViewConfigStockRequest productLevel3ViewConfigStockRequest);

  @RequestLine("POST /api/product-level3/getL5DetailsByProductSku?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemPickupPointListingL3Response> getItemPickupPointL3Listing(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request);

  @RequestLine("POST /api/product-level3/filter/bulk-new-download-summary?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&businessPartnerCode={businessPartnerCode"
      + "}&fetchB2bData={fetchB2bData}&fetchViewConfigByChannel={fetchViewConfigByChannel}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BulkDownloadProductLevel3Response> bulkDownloadSummaryFromDb(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("businessPartnerCode") String businessPartnerCode,
      @Param("fetchB2bData") boolean fetchB2bData, ProductLevel3SummaryRequest request,
      @Param("fetchViewConfigByChannel") String fetchViewConfigByChannel);

  @RequestLine("PUT /api/product-level3/v2/{productSku}/listing-update?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> listingPartialUpdate(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productSku") String productSku, ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request);

  @RequestLine(
    "POST /api/product-level3/v2/getL5SummaryByProductSkus?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&page={page}&size={size}"
      + "&businessPartnerCode={businessPartnerCode}&onlineOrCnc={onlineOrCnc}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductLevel3SummaryResponse> getL5SummaryByProductSkuList(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("page") int page, @Param("size") int size,
    @Param("businessPartnerCode") String businessPartnerCode,
    @Param("onlineOrCnc") boolean onlineOrCnc, SimpleListStringRequest simpleListStringRequest);

  @RequestLine("POST /api/product/create-new-product?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse createNewProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ProductCreationRequest productCreationRequest);

  @RequestLine(
      "POST /api/product-level3/getL5DetailsByProductSku?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&page={page}&size={size}&onlyDefaultViewConfig={onlyDefaultViewConfig}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemPickupPointListingL3Response> getItemPickupPointListingL3Response(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("page") int page,
      @Param("size") int size, @Param("onlyDefaultViewConfig") boolean onlyDefaultViewConfig,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request);

  @RequestLine(
      "POST /api/product/edit-price-stock-variants-info?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> updateSummaryL5(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductVariantUpdateRequest productVariantUpdateRequest);

  @RequestLine("GET /api/product/delete-terminated-seller-products/{productSku}?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}" + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse deleteTerminatedSellerProducts(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku);

  @RequestLine(
      "POST /api/product-level3/v2/deleteL5ByPickupPointCode?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<DeleteInProgressL5Response> deleteL5ByPickupPointCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ItemSkuPickupPointRequest itemSkuPickupPointRequest);

  @RequestLine("GET /api/product-level3/fetch-in-progress-l4-by-pickpointcode?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&businessPartnerCode={businessPartnerCode}&pickupPointCode={pickupPointCode}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<InProgressProductsByPickupPointCodeResponse> getInProgressProductsByPickupPointCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("businessPartnerCode") String businessPartnerCode, @Param("pickupPointCode") String pickupPointCode,
      @Param("page") int page, @Param("size") int size);

  @RequestLine(
    "POST /api/product-level3/v2/createDefaultL5Fbb?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<FbbCreatePickupPointResponse> createDefaultL5Fbb(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, FbbCreatePickupPointRequest itemSkuPickupPointRequest);

  @RequestLine("POST /api/product-level3/v2/{productCode}/updateBrand?storeId={storeId}&channelId={channelId"
          + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateBrandOfProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, BrandUpdateRequest brandUpdateRequest);

  @RequestLine("POST /api/product/updateBrand?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductBrandName(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      com.gdn.mta.bulk.dto.ProductBrandUpdateRequest productBrandUpdateRequest);

  @RequestLine("GET /api/product/detail/filter/product-code/{productCode}?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&inAllProducts={inAllProducts}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductDetailResponse> filterProductDetailByProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("inAllProducts") boolean inAllProducts,
      @Param("productCode") String productCode);

  @RequestLine("POST /api/product/update?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      ProductRequest productRequest);

  @RequestLine("GET /api/product/generate/barcode?" + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<String> generateBarcode(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username);

  @RequestLine("POST /api/product/create?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&validateCategory={validateCategory}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<String> create(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("validateCategory") Boolean validateCategory, CreateProductRequest createProductRequest);

  @RequestLine("POST /api/product/updateActivatedProductsBulkMasterData?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BulkMasterProductUpdateResponse> updateActivatedProductsBulkMasterData(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, BulkMasterProductUpdateRequest request);

  @RequestLine("GET /api/product/generate-product-code?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<String> generateProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username);

  @RequestLine("GET /api/product/collection/filter/keyword/bulk-download?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&categoryCode={categoryCode}&keyword={keyword}&reviewPending={reviewPending}&sortBy={sortBy}&activated={activated}&viewable={viewable}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductCodesResponse> filterProductCollectionSummaryByKeywordforBulkDownload(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("categoryCode") String categoryCode, @Param("keyword") String keyword,
      @Param("reviewPending") Boolean reviewPending, @Param("sortBy") String sortBy,
      @Param("activated") boolean activated, @Param("viewable") boolean viewable);

  @RequestLine("POST /api/product/get-distinct-business-partner-code-for-revised-products?storeId={storeId}&requestId={requestId}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<BusinessPartnerCodeResponseList> getDistinctBusinessPartnerCodeForRevisedProducts(
      @Param("storeId") String storeId, @Param("requestId") String requestId, @Param("page") int page,
      @Param("size") int size, NeedRevisionProductsRequest needRevisionProductsRequest);

  @RequestLine("GET /api/product/get-all-products-by-business-partner-code?storeId={storeId}&requestId={requestId}&businessPartnerCode={businessPartnerCode}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductCodeAndNameDetails> getProductsByBusinessPartnerCodeFromLastXDays(
      @Param("storeId") String storeId, @Param("requestId") String requestId,
      @Param("businessPartnerCode") String businessPartnerCode, @Param("page") int page,
      @Param("size") int size);

  @RequestLine("POST /api/product-level3/v2/eligible-for-need-revision-deletion?storeId={storeId"
    + "}&requestId={requestId}&channelId={channelId}&clientId={clientId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<NeedRevisionEligibilityResponse> eligibilityForNeedRevisionDeletion(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, List<NeedRevisionEligibilityRequest> needRevisionEligibilityRequest);

  @RequestLine("POST /api/product/delete-product-collection?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&needEmailNotification={needEmailNotification}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse deleteProductCollection(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    @Param("needEmailNotification") boolean needEmailNotification,
    DeleteProductRequest deleteProductRequest);

  @RequestLine("POST /api/generator/generate/shipping-weight?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<GenerateShippingWeightResponse> generateShippingWeight(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, GenerateShippingWeightRequest request);

  @RequestLine(
      "POST /api/offline-item/upsert?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<UpsertOfflineItemResponse> upsertOfflineItems(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("merchantCode") String merchantCode,
      ListRequestDTO<UpsertOfflineItemRequest> request);

  @RequestLine(
      "POST /api/product-business-partner/save-activated-false-return-id?storeId={storeId"
          + "}&channelId"
          + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<String> saveProductBusinessPartnerWithActivatedFalseReturnId(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductBusinessPartnerRequest request) throws Exception;

  @RequestLine("GET /api/product-business-partner/{id}?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductBusinessPartnerResponse> getProductBusinessPartner(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("id") String id) throws Exception;

  @RequestLine("GET /api/offline-item/filter/summary/instant-pickup/bulk-download?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}"
      + "&username={username}&merchantCode={merchantCode}&pickupPointCode={pickupPointCode}"
      + "&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<OfflineItemInstantPickupBulkDownloadResponse> filterSummaryInstantPickupBulkDownload(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("merchantCode") String merchantCode, @Param("pickupPointCode") String pickupPointCode,
      @Param("page") int page, @Param("size") int size);

  @RequestLine(
      "POST /api/product-level3/filter/bulk-download-summary?storeId={storeId}&channelId={channelId}"
          + "&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size"
          + "={size}&businessPartnerCode={businessPartnerCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BulkDownloadProductLevel3Response> filterSummaryForBulkDownload(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("businessPartnerCode") String businessPartnerCode,
      @Param("page") int page, @Param("size") int size,
      ProductLevel3SummaryRequest request);

  @RequestLine("POST /api/product-level3/filter/summarybyCategoryAndBrand?storeId={storeId"
      + "}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username"
      + "}&page={page}&size={size}&orderBy={orderBy}&sortBy={sortBy}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductLevel3SummaryResponse> filterSummaryByCategoryAndBrand(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      @Param("orderBy") String orderBy, @Param("sortBy") String sortBy,
      BrandAndCategoryItemSummaryRequest request);

  @RequestLine("POST /api/product-level3/doBulkSuspensionAction?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<SuspensionProductResponse> doBulkSuspensionProductsActions(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SuspensionProductRequestList request);

  @RequestLine("POST /api/product-level3/v2/product/{productSku}/master"
      + "-data-edit-info?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId"
      + "={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updatedProductMaterData(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productSku") String productSku, ProductMasterDataEditRequest masterDataEditRequest);

  @RequestLine(
      "GET /api/product/getProductsByBrandName" + "?storeId={storeId}" + "&channelId={channelId}"
          + "&clientId={clientId}" + "&requestId={requestId}" + "&username={username}"
          + "&brandName={brandName}" + "&page={page}" + "&size={size}")
  @Headers("Accept: application/json")
  GdnRestListResponse<ProductAndBrandResponse> getProductsByBrandName(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("brandName") String brandName,
      @Param("page") int page, @Param("size") int size
  );
}
