package com.gdn.partners.pbp.outbound.xProduct.feign;

import com.gda.mta.product.dto.NeedCorrectionProductActivationRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.model.vo.SimpleStringBooleanMapRequest;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.CogsUpdateListRequest;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteOfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemImagesListResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.util.GdnRestSimpleResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

public interface XProductFeign {

  @RequestLine("POST /api/product/update?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}&isOnlyExternal={isOnlyExternal}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductResponse> updateProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("isOnlyExternal") boolean isOnlyExternal, ProductRequest productRequest);


  @RequestLine("POST /api/item/update?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}&isOnlyExternal={isOnlyExternal}&isProductTypeChanged={isProductTypeChanged}"
      + "&isPreOrderChanged={isPreOrderChanged}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ItemResponse> updateItem(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("isOnlyExternal") boolean isOnlyExternal, @Param("isProductTypeChanged") boolean isProductTypeChanged,
      @Param("isPreOrderChanged") boolean isPreOrderChanged, ItemRequest itemRequest);

  @RequestLine(
      "POST /api/item/updateViewConfigAndForceReview?storeId={storeId}&channelId={channelId}&clientId={clientId"
          + "}&requestId={requestId}&username={username}&forceReview={forceReview}"
          + "&isArchive={isArchive}&scheduleRemoval={scheduleRemoval}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateItemViewConfigAndForceReview(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("forceReview") boolean forceReview,
      @Param("isArchive") boolean isArchive, @Param("scheduleRemoval") boolean scheduleRemoval,
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuListRequest);

  @RequestLine("GET /api/product/getProductAndItems?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&showDeleted={showDeleted}"
      + "&productSku={productSku}&combineOthersBundlings=false&includeForceReview={includeForceReview}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("showDeleted") boolean showDeleted,
      @Param("productSku") String productSku, @Param("includeForceReview") boolean includeForceReview);

  @RequestLine("GET /api/product/getProductAndItems?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&showDeleted={showDeleted}"
      + "&productSku={productSku}&combineOthersBundlings=false&includeForceReview"
      + "={includeForceReview}&needProductData={needProductData}&isMigrateAndSyncProduct={isMigrateAndSyncProduct}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("showDeleted") boolean showDeleted,
      @Param("productSku") String productSku, @Param("includeForceReview") boolean includeForceReview,
      @Param("needProductData") boolean needProductData,
      @Param("isMigrateAndSyncProduct") boolean isMigrateAndSyncProduct);

  @RequestLine("POST /api/offlineItem/deleteBulkOfflineItem?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<DeleteOfflineItemResponse> bulkDeleteOfflineItem(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("merchantCode") String merchantCode, List<DeleteOfflineItemRequest> deleteOfflineItemRequests);

  @RequestLine("PUT /api/product/migrateProductSku?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&productSku={productSku}&newProductCode={newProductCode}&rollback={rollback}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  public GdnBaseRestResponse updateMigratedProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku,
      @Param("newProductCode") String newProductCode, @Param("rollback") boolean rollback);

  @RequestLine("POST /api/businessPartner/getListByBusinessPartnerCodes?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<BusinessPartnerResponse> getBusinessPartnerDetails(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SimpleListStringRequest businessPartnerCodes);

  @RequestLine("POST /api/summary/productSkuList?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}"
      + "&businessPartnerCode={businessPartnerCode}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductSkuSummaryResponse> getProductSkuSummary(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("businessPartnerCode") String businessPartnerCode,
      @Param("page") int page, @Param("size") int size,
      ProductSkuSummaryRequest productSkuSummaryRequest);

  @RequestLine("POST /api/item/update-pickup-point-codes?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleLongResponse> updatePickupPointCodes(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      PickupPointUpdateRequest pickupPointUpdateRequest);

  @RequestLine("POST /api/summary/filter/getItemsSummaryDetailByFilter?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page"
      + "}&size={size}&orderBy={orderBy}&sortBy={sortBy}"
      + "&fetchViewConfigByChannel={fetchViewConfigByChannel}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemSummaryDetailResponse> getItemsSummaryDetailByFilter(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      @Param("fetchViewConfigByChannel") String fetchViewConfigByChannel,
      ItemsSummaryDetailRequest itemFilterRequest);

  @RequestLine(
      "GET /api/product/getProductDetailsByProductSku?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductL3Response> getProductDetailsByProductSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku);

  @RequestLine("POST /api/summary/{productSku}/listing-update?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateItemListing(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productSku") String productSku, ItemListingUpdateRequest itemListingUpdateRequest);

  @RequestLine("POST /api/item/getL5ItemListByProductSku?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&fetchProductData={fetchProductData}&fetchB2bData={fetchB2bData"
      + "}&fetchViewConfigByChannel={fetchViewConfigByChannel}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemLevel5Response> getL5ItemList(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("fetchProductData") boolean fetchProductData, @Param("fetchB2bData") boolean fetchB2bData,
      @Param("fetchViewConfigByChannel") String fetchViewConfigByChannel, ItemLevel4ListingWebRequest request);

  @RequestLine(
      "GET /api/product/{productSku}/getPickupPointCodes?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductPickupPointListResponse> getPickupPointCodesByProductSku(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("productSku") String productSku);

  @RequestLine("POST /api/product/{productSku}/archive?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&doArchive={doArchive}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse toggleArchiveProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("doArchive") boolean doArchive, @Param("productSku") String productSku);

  @RequestLine(
      "POST /api/product/bulk-update-off2On-by-product-skus?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleListStringResponse> bulkUpdateOff2OnActiveFlagByProductSkus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SimpleStringBooleanMapRequest request);

  @RequestLine(
      "GET /api/productList/generateProductScoreByProductSkuOrProductCode?storeId={storeId}&channelId={channelId}"
          + "&clientId={clientId}&requestId={requestId}&username={username}&productSku={productSku}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<L3VersionResponse> generateProductScoreByProductSkuOrProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku,
      @Param("productCode") String productCode);

  @RequestLine(
      "GET /api/productList/generateProductScoreByProductSkuOrProductCode?storeId={storeId}&channelId={channelId}"
          + "&clientId={clientId}&requestId={requestId}&username={username}&updateCategory={updateCategory}&productSku={productSku}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<L3VersionResponse> generateProductScoreByProductSkuOrProductCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("updateCategory") boolean updateCategory, @Param("productSku") String productSku,
      @Param("productCode") String productCode);

  @RequestLine("GET /api/item/{productSku}/item-pickup-points?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&fbbActivated={fbbActivated}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemPickupPointCodeResponse> getItemPickupPointCodeByProductSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      @Param("productSku") String productSku, @Param("fbbActivated") boolean fbbActivated);

  @RequestLine(
      "GET /api/product-v2/{merchantCode}/validateDuplicateProductBySellerSku?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}&sellerSku={sellerSku}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<DuplicateProductDetailsResponse> validateDuplicateProductBySellerSku(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("sellerSku") String sellerSku,
      @Param("merchantCode") String merchantCode);

  @RequestLine("POST /api/product-v2/getProductL3DetailByProductSkuOrProductCode?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PrdProductResponse> getPrdProductDetailByProductSkuOrProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductSkuAndProductCodeRequest request);

  @RequestLine("POST /api/item/update-content-change?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&productSku={productSku}&contentChange={contentChange}&publishItems={publishItems}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateContentChange(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productSku") String productSku, @Param("contentChange") boolean contentChange,
      @Param("publishItems") boolean publishItems);

  @RequestLine("GET /api/product/getProductCountByType?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&type={type}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductCountResponse> getProductCountByType(@Param("storeId") String storeId,
      @Param("channelId")  String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("type") String type, @Param("merchantCode") String merchantCode);

  @RequestLine("GET /api/product/getProductAndSingleItem?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&itemSku={itemSku}&convertPreOrderDetails={convertPreOrderDetails}&"
      + "fetchMfdTrueItem={fetchMfdTrueItem}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndSingleItemByItemSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("itemSku") String itemSku,
      @Param("convertPreOrderDetails") boolean convertPreOrderDetails,
      @Param("fetchMfdTrueItem") boolean fetchMfdTrueItem);

  @RequestLine("POST /api/product/update-edited-product?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}&updateCategory={updateCategory}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateEditedProduct(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("updateCategory") boolean updateCategory, ProductEditRequest productEditRequest);

  @RequestLine(
      "POST /api/item/get-item-images-by-item-skus?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemImagesListResponse> getListOfImagesByItemSkus(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      SimpleSetStringRequest itemSkusSet);

  @RequestLine("PUT /api/product/activate-need-correction?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<ActivateNeedRevisionResponse> activateOnNeedCorrection(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, NeedCorrectionProductActivationRequest activationRequest);

  @RequestLine("GET /api/product/get-product-type-by-product-code/{productCode}?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductTypeResponse> getProductDetailsByProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine(
      "POST /api/summary/updateWholeSaleActivationFlag?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}&wholeSalePriceActivated={wholeSalePriceActivated}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateWholeSaleActivationFlag(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("wholeSalePriceActivated") boolean wholeSalePriceActivated,
      SimpleListStringRequest itemSkus);

  @RequestLine(
    "POST /api/businessPartnerPickupPoint/detail?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
    GdnRestListResponse<BusinessPartnerPickupPointResponse> getBusinessPartnerPickupPointDetails(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, SimpleListStringRequest pickupPointCodes);

  @RequestLine(
      "POST /api/item/getItemPickupPointsByItemSku?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemSkuPickupPointCodeResponse> getItemPickupPointCodeByItemSkus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SimpleListStringRequest itemSkusList);

  @RequestLine(
    "POST /api/itemPickupPoint/getItemSummaryByItemSkuAndPickupPointCodeList?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&fetchViewConfigByChannel={fetchViewConfigByChannel}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemSummaryListResponse> findItemSummaryByItemSkuAndPickupPointCode(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("fetchViewConfigByChannel") String fetchViewConfigByChannel,
      List<ItemPickupPointRequest> itemPickupPointRequest);

  @RequestLine(
    "POST /api/v2/summary/{productSku}/listing-update?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&"
      + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateItemPickupPointListing(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    @Param("productSku") String productSku,
    ItemPickupPointListingUpdateRequest pickupPointListingUpdateRequest);

  @RequestLine("GET /api/productList/updateMasterDataFieldsInProduct?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateMasterDataFieldsInProduct(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku);

  @RequestLine("POST /api/product/addProductAndItems?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AddProductAndItemsResponse> addProductAndItems(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductAndItemActivationRequest productAndItemActivationRequest);

  @RequestLine("POST /api/summary/getItemNameByItemSku?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&includeMarkForDelete={includeMarkForDelete}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleMapStringResponse> getItemNameByItemSkus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("includeMarkForDelete") boolean includeMarkForDelete, SimpleListStringRequest itemSkus);

  @RequestLine("POST /api/listing/itemPickupPointListing?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&page={page}&size={size"
      + "}&fetchViewConfigByChannel={fetchViewConfigByChannel}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemPickupPointListingResponse> getItemPickupPointList(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      ItemPickupPointListingRequest itemPickupPointListingRequest,
      @Param("fetchViewConfigByChannel") String fetchViewConfigByChannel);

  @RequestLine("POST /api/summary/getPickupPointDetailsFromPickupPointCodes?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
    + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PickupPointDetailResponse> getPickupPointDetailByCodes(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, SimpleListStringRequest pickupPointCodes);

  @RequestLine(
      "POST /api/itemPickupPoint/update-item-pickup-points?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<EditItemResponse> updateItemPickupPoints(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ItemPickupPointUpdateRequest itemPickupPointUpdateRequest);

  @RequestLine("POST /api/listing/getItemPickupPointSummary?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
    + "requestId={requestId}&username={username}&page={page}&size={size}&fetchViewConfigByChannel={fetchViewConfigByChannel}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemResponseV2> getItemPickupPointSummary(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    @Param("page") int page, @Param("size") int size, @Param("fetchViewConfigByChannel") String fetchViewConfigByChannel,
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest);

  @RequestLine(value = "POST /api/summary/filter/L3?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductL3SummaryResponse> getFilterSummaryL3(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      ProductSummaryRequest productSummaryRequest);

  @RequestLine(
    "GET /api/listing/getProductSummary?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductSummaryResponseV2> getProductSummary(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    @Param("page") int page, @Param("size") int size,
    ProductSummaryRequestV2 productSummaryRequestV2);

  @RequestLine(
      "POST /api/listing/getItemPickupPointsByItemSku?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemResponseV2> getItemPickupPointsByItemSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size, ItemRequestV2 itemRequestV2);

  @RequestLine(
      "POST /api/product-v2/updateCncActivatedFlagByItemSkus?storeId={storeId}&channelId={channelId}&clientId"
          + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateCncActivationFlag(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      SimpleListStringRequest simpleListStringRequest);

  @RequestLine(
    "POST /api/itemPickupPoint/deleteItemPickupPointByPickupPointCode?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<DeleteItemPickupPointResponse> deleteActiveItemPickupPointByPickupPointCode(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    DeleteItemPickupPointRequest deleteItemPickupPointRequest);

  @RequestLine("POST /api/itemPickupPoint/createFbbPickupPoint?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CreateFbbPickupPointResponse> createFbbPickupPoint(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, CreateFbbPickupPointRequest createFbbPickupPointRequest);

  @RequestLine("GET /api/itemPickupPoint/{productCode}/getMinAndMaxOfferPrice?storeId={storeId}&channelId={channelId}"
          + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<MinMaxItemPriceResponse> getMinAndMaxOfferPrice(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);

  @RequestLine(
    "POST /api/itemPickupPoint/getProductL5DetailByItemSkuAndPickupPointCodeList?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&fetchViewConfigByChannel={fetchViewConfigByChannel}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductL5DetailResponse> findProductAndItemByItemSkuAndPickupPointCode(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("fetchViewConfigByChannel") String fetchViewConfigByChannel,
      List<ItemPickupPointRequest> itemPickupPointRequest);

  @RequestLine("GET /api/item/get-item-basic-details-by-product-sku?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetailsByProductSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku);

  @RequestLine("POST /api/product-v2/{productSku}/basicProductInfo?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BasicProductResponse> basicProductInfo(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productSku") String productSku);

  @RequestLine("GET /api/product-v2/{productSku}/basicProductInfo?storeId={storeId}&channelId={channelId}&"
      + "clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BasicProductResponse> getBasicProductInfo(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productSku") String productSku);


  @RequestLine(
    "POST /api/product-v2/{productSku}/update-product-item-pickup-point?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&productSku={productSku}"
      + "&updateCategory={updateCategory}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CombinedEditItemResponse> updateEditedProductAndItemPickupPoint(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("updateCategory") boolean updateCategory,
    @Param("productSku") String productSku,
    ProductDetailPageEditRequest productDetailPageEditRequest);

  @RequestLine("POST /api/offlineItem/findByMerchantCodeAndMerchantSkus?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<OfflineItemResponse> getOfflineItemsByMerchantCodeAndMerchantSkus(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("merchantCode") String merchantCode, SimpleListStringRequest merchantSkuRequest);

  @RequestLine("POST /api/offlineItem/upsertOfflineItemPrice?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<UpsertOfflineItemPriceResponse> upsertOfflineItemPrice(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("merchantCode") String merchantCode,
      List<OfflineItemRequest> offlineItemRequest);

  @RequestLine("POST /api/summary/filter?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&page={page}&size={size}&orderBy={orderBy}&sortBy={sortBy}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemSummaryResponse> getListOfItemSummaryByFilter(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      @Param("orderBy") String orderBy, @Param("sortBy") String sortBy, ItemSummaryRequest itemSummaryRequest);

  @RequestLine("POST /api/product/updateMasterCatalog?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductMasterCatalog(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, MasterCatalogRequest masterCatalogRequest);

  @RequestLine("POST /api/product/addProductAttribute?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse addProductAttribute(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("productCode") String productCode, ProductAttributeRequest productAttributeRequest);

  @RequestLine("POST /api/offlineItem/upsertOfflineItem?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<UpsertOfflineItemPriceResponse> upsertOfflineItem(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("merchantCode") String merchantCode, List<UpsertOfflineItemRequest> upsertOfflineItemRequests);

  @RequestLine("POST /api/product-v2/getProductBasicDetailsByProductSku?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductBasicResponse> getProductBasicDetails(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @RequestBody SimpleListStringRequest request);

  @RequestLine("POST /api/item/getBulkItemDetailByItemSkus?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&fetchBundleRecipe={fetchBundleRecipe}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetails(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("fetchBundleRecipe") boolean fetchBundleRecipe,
      @RequestBody SimpleListStringRequest itemSkus);

  @RequestLine("POST /api/item/get-item-basic-details-by-item-skus?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&inAllProducts={inAllProducts}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetailsByItemSkus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("inAllProducts") boolean inAllProducts,
      SimpleListStringRequest itemSkuList);

  @RequestLine("GET /api/product/getSecondaryFilterCounts?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&type={type}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductCountResponse> getSecondaryCounts(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("type") String type, @Param("merchantCode") String merchantCode);

  @RequestLine("GET /api/itemPickupPoint/{itemSku}/get-L5-count-by-item-sku?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&itemSku={itemSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleLongResponse> getL5CountByItemSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("itemSku") String itemSku);

  @RequestLine("POST /api/product/{productSku}/reconcile-product-variants?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemPickupPointCodeResponse> reconcileProductVariants(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku,
      AddDeleteVariantRetryRequest addDeleteVariantRetryRequest);

  @RequestLine("POST /api/item/get-shared-product-bundle-recipe?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<SharedProductBundleRecipeResponse> getSharedProductBundleRecipeDetails(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SimpleSetStringRequest request);

  @RequestLine(
    "GET /api/product/getProductDetailsForProductCenter?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductCenterDetailResponse> getProductSkuDetailResponse(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("productSku") String productSku);


  @RequestLine("POST /api/itemPickupPoint/fetchBasicDetailsByItemSkuAndPickupPointCodeList?storeId={storeId}&channelId={channelId}"
    + "&clientId={clientId}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemPickupPointBasicResponse> fetchBasicDetailsByItemSkuAndPickupPointCodeList(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    List<ItemPickupPointRequest> request);

  @RequestLine("POST /api/product-v2/migrateProductAndL5DetailsByProductSku?storeId={storeId}"
    + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse migrateProductAndL5DetailByProductSku(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    ProductAndL5MigrationRequest request);

  @RequestLine("GET /api/offlineItem/findByMerchantCodeAndItemSku?storeId={storeId}&channelId={channelId}"
          + "&clientId={clientId}&requestId={requestId}&username={username}&merchantCode={merchantCode}&itemSku={itemSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<OfflineItemPriceResponse> findOfflinePriceByMerchantCodeAndItemSku(
          @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
          @Param("requestId") String requestId, @Param("username") String username,
          @Param("merchantCode") String merchantCode, @Param("itemSku") String itemSku);

  @RequestLine("POST /api/offlineItem/updatePriceByItemSku?storeId={storeId}&channelId={channelId}"
          + "&clientId={clientId}&requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateOfflineItemPriceByItemSku(
          @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
          @Param("requestId") String requestId, @Param("username") String username,
          @Param("merchantCode") String merchantCode, UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest);

  @RequestLine(
      "POST /api/summary/getProductNameByProductSku?storeId={storeId}&channelId={channelId"
          + "}&clientId={clientId}&"
          + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleMapStringResponse> getProductNameByProductSku(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SimpleListStringRequest productSkus);


  @RequestLine("GET /api/product/getProductAndSingleItemForActiveAndInactiveItems?storeId={storeId"
      + "}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&itemSku={itemSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductAndItemsResponse> getProductDetailAndSingleItemByItemSku(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("itemSku") String itemSku);

  @RequestLine("GET api/product/getProductCountByBrand?storeId={storeId" + "}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&brand={brand}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleLongResponse> getProductsCountByBrand(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("brand") String brand);

  @RequestLine("POST /api/item/updateViewConfig?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&itemSku={itemSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateItemViewConfig(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("itemSku") String itemSku, ItemViewConfigRequest itemViewConfigRequest);

  @RequestLine("POST /api/product/unsynchronize?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&productSku={productSku"
      + "}&overwriteExistingMasterData={overwriteExistingMasterData}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnRestSingleResponse<ProductAndItemsResponse> unsynchronizeProduct(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku,
      @Param("overwriteExistingMasterData") boolean overwriteExistingMasterData);

  @RequestLine("POST /api/product/synchronize?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnRestSingleResponse<ProductAndItemsResponse> synchronizeProduct(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku);


  @RequestLine("GET /api/summary/getSingleItemSummaryByItemSku?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}&itemSku"
      + "={itemSku}")
  @Headers("Accept: application/json")
  GdnRestSingleResponse<ItemSummaryResponse> getItemSummaryByItemSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("itemSku") String itemSku);

  @RequestLine("GET /api/summary/getSingleArchivedItemSummaryByItemSku?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}&itemSku"
      + "={itemSku}")
  @Headers("Accept: application/json")
  GdnRestSingleResponse<ItemSummaryResponse> getArchivedItemSummaryByItemSku(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("itemSku") String itemSku);

  @RequestLine("POST /api/off2On/activate?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}&itemSku"
      + "={itemSku}")
  @Headers("Accept: application/json")
  GdnBaseRestResponse activateOff2OnChannelActive(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("itemSku") String itemSku);

  @RequestLine("POST  /api/off2On/deactivate?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}&itemSku"
      + "={itemSku}")
  @Headers("Accept: application/json")
  GdnBaseRestResponse deactivateOff2OnChannelActive(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("itemSku") String itemSku);

  @RequestLine("POST /api/item/archive?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&itemSku={itemSku}&doArchive"
      + "={doArchive}")
  @Headers("Accept: application/json")
  GdnBaseRestResponse toggleArchiveItem(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("itemSku") String itemSku, @Param("doArchive") boolean doArchive);

  @RequestLine("POST api/product/suspend?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&productSku={productSku"
      + "}&suspendProduct={suspendProduct}")
  @Headers("Accept: application/json")
  GdnBaseRestResponse toggleSuspensionProduct(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("productSku") String productSku, @Param("suspendProduct") boolean suspendProduct);


  @RequestLine("POST api/item/updateResignMerchantItemsByMerchantCode?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}&merchantCode"
      + "={merchantCode}")
  @Headers("Accept: application/json")
  GdnBaseRestResponse updateResignMerchantItemsByMerchantCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("merchantCode") String merchantCode);

  @RequestLine("GET /api/item/get-item-skus-by-item-code?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&itemCode={itemCode}")
  @Headers("Accept: application/json")
  GdnRestListResponse<ItemPriceResponse> getItemSkusByItemCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("itemCode") String itemCode);

  @RequestLine("POST /api/productList/getProductList?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnRestListResponse<ActiveProductResponse> getAllProducts(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("page") int page, @Param("size") int size, ActiveProductRequest activeProductRequest);

  @RequestLine("POST /api/productList/getSuspendedItemList?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnRestListResponse<ItemSummaryResponse> getSuspendedItemList(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("page") int page, @Param("size") int size, ActiveProductRequest activeProductRequest);

  @RequestLine("GET /api/product/getProductCodeByProductCodeAndMerchantCode?storeId"
      + "={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username"
      + "={username}&productCode={productCode}&merchantCode={merchantCode}")
  @Headers("Accept: application/json")
  GdnRestListResponse<ProductResponse> getProductsByProductCodeAndMerchantCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode,
      @Param("merchantCode") String merchantCode);


  @RequestLine("GET /api/item/get?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&itemSku={itemSku}")
  @Headers("Accept: application/json")
  GdnRestSingleResponse<ItemResponse> getItem(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("itemSku") String itemSku);

  @RequestLine("GET /api/item/isPickupPointCodeUsed?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username"
      + "}&pickupPointCode={pickupPointCode}")
  @Headers("Accept: application/json")
  GdnRestSingleResponse<SimpleBooleanResponse> isPickupPointCodeUsed(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("pickupPointCode") String pickupPointCode);

  @RequestLine("POST /api/summary/update?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&itemSku={itemSku"
      + "}&merchantCode={merchantCode}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnRestSingleResponse<ItemSummaryResponse> updateItemSummary(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("itemSku") String itemSku, @Param("merchantCode") String merchantCode,
      UpdateItemSummaryRequest updateItemSummaryRequest);

  @RequestLine("POST /api/item/updatePrice?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&itemSku={itemSku}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnBaseRestResponse updateItemPrice(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("itemSku") String itemSku, PriceRequest priceRequest);

  @RequestLine("POST /api/offlineItem/deleteOfflineItem?storeId={storeId}&channelId"
      + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}&merchantCode"
      + "={merchantCode}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnBaseRestResponse deleteOfflineItem(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("merchantCode") String merchantCode,
      List<DeleteOfflineItemRequest> deleteOfflineItemRequests);

  @RequestLine("POST /api/summary/filterbyCategoryAndBrand?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size"
      + "}&orderBy={orderBy}&sortBy={sortBy}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnRestListResponse<ItemSummaryResponse> getListOfItemSummaryByCategoryAndBrandFilter(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      @Param("orderBy") String orderBy, @Param("sortBy") String sortBy,
      CampaignItemSummaryRequest campaignItemSummaryRequest);

  @RequestLine("POST /api/product/get-campaign-eligibility-for-product-skus"
      + "?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Accept: application/json", "Content-Type: application/json"})
  GdnRestSingleResponse<PromoEligibilityResponse> getCampaignEligibilityForProductSkus(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, PromoEligibilityRequest promoEligibilityRequest);

  @RequestLine(
    "POST /api/itemPickupPoint/findByItemSkus?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemPickupPointL5Response> findByItemSkus(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("page") int page, @Param("size") int size,
    SimpleListStringRequest simpleListStringRequest);

  @RequestLine("POST /api/product-v2/updateProductMasterDataInfo?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateProductMasterDataInfo(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    ProductBasicMasterFieldsRequest request);

  @RequestLine(
    "GET /api/itemPickupPoint/getCncAtL5ByProductSku?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSimpleResponse<SimpleBooleanResponse> getCncAtL5ByProductSku(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("productSku") String productSku);

  @RequestLine("POST /api/product-v2/{productSku}/update-cogs?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateCogsValue(@Param("productSku") String productSku, @Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, CogsUpdateListRequest request);

  @RequestLine("GET /api/product-v2/{productSku}/cogs?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CogsResponse> getCogsData(@Param("productSku") String productSku,
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("page") int page,
      @Param("size") int size);

}
