package com.gdn.mta.bulk.feignConfig;

import java.util.List;
import java.util.Set;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.ItemBasicL4Response;
import com.gdn.mta.bulk.models.ProductL3Response;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductSkuResponse;
import com.gdn.x.product.model.vo.SimpleStringBooleanMapRequest;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequestV2;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface XProductFeign {

  @RequestLine("POST /api/product/getUnmappedProductSkus?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<UnmappedSkuResponse> getUnmappedProductSkus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, List<String> categoryCodes);

  @RequestLine("POST /api/product/bulk-update-off2On-by-product-skus?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}"
      + "&username={username}&updateOff2OnHistory={updateOff2OnHistory}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleListStringResponse> bulkUpdateOff2OnActiveFlagByProductSkus(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("updateOff2OnHistory") Boolean updateOff2OnHistory, SimpleStringBooleanMapRequest request);

  @RequestLine("POST /api/summary/filter/L3?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}"
      + "&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductL3SummaryResponse> getFilterSummaryL3(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("page") int page, @Param("size") int size,
      ProductSummaryRequest productSummaryRequest);

  @RequestLine("POST /api/item/get-basic-item-details?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}"
      + "&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemBasicL4Response> getL4ItemListByProductSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") Integer page, @Param("size") Integer size,
      ItemLevel4ListingWebRequest requestBody);

  @RequestLine("POST /api/item/getProductSkusAndMerchantTypeByItemCodes?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemCodeDetailResponse> getItemDetailsByItemCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SimpleSetStringRequest itemCodesSet);

  @RequestLine("POST /api/summary/filter?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}"
    + "&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemSummaryResponse> getItemSummaryByFilter(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    @Param("page") int page, @Param("size") int size,
    ItemSummaryRequest itemFilterRequest);

  @RequestLine("POST /api/item/{itemSku}/updateItemViewConfig?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateItemViewConfigWithItemStatus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("itemSku") String itemSku,
      ItemViewConfigBaseRequest itemViewConfigBaseRequest);

  @RequestLine("POST /api/itemPickupPoint/{productSku}/updateItemPickupPointViewConfig?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse updateItemPickupPointViewConfigWithProductStatus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku,
      ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest);

  @RequestLine("GET /api/product/getProductAndItems?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&showDeleted={showDeleted}"
      + "&productSku={productSku}&combineOthersBundlings=false&includeForceReview={includeForceReview}&needProductData={needProductData}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItems(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("showDeleted") boolean showDeleted,
      @Param("productSku") String productSku, @Param("includeForceReview") boolean includeForceReview,
      @Param("needProductData") boolean needProductData);

  @RequestLine("POST /api/product-v2/getProductAndItemInfo?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductAndItemInfoResponseV2> getProductInfoByItemSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, GetProductInfoRequestV2 request);

  @RequestLine("POST /api/product/addSalesCategory?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&catalogCode={catalogCode}&newCategoryCode={newCategoryCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse addProductSalesCatalog(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("catalogCode") String catalogCode, @Param("newCategoryCode") String newCategoryCode,
      SimpleListStringRequest productSkus);

  @RequestLine("POST /api/product/deleteSalesCategory?storeId={storeId}&channelId={channelId}&clientId"
      + "={clientId}&requestId={requestId}&username={username}&catalogCode={catalogCode}&oldCategoryCode={oldCategoryCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse deleteSalesCatalog(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("catalogCode") String catalogCode, @Param("oldCategoryCode") String oldCategoryCode,
      SimpleListStringRequest productSkus);

  @RequestLine("POST /api/product/{productSku}/archive?storeId={storeId}&channelId={channelId}"
    + "&clientId={clientId}&requestId={requestId}&username={username}&doArchive={doArchive}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse toggleArchiveProduct(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    @Param("doArchive") boolean doArchive, @Param("productSku") String productSku);

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

  @RequestLine("POST /api/listing/itemPickupPointListing?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
    + "requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemPickupPointListingResponse> getItemPickupPointList(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("page") int page, @Param("size") int size,
    ItemPickupPointListingRequest itemPickupPointListingRequest);

  @RequestLine(
      "POST /api/item/getItemPickupPointsByItemSku?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemSkuPickupPointCodeResponse> getItemPickupPointCodeByItemSkus(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, SimpleListStringRequest itemSkusList);

  @RequestLine(
    "POST /api/itemPickupPoint/getItemSummaryByItemSkuAndPickupPointCodeList?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemSummaryListResponse> getItemSummaryByItemSkuAndPPCode(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, List<ItemPickupPointRequest> itemPickupPointRequest);

  @RequestLine(
      "POST /api/itemPickupPoint/deleteItemPickupPointByPickupPointCode?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<DeleteItemPickupPointResponse> deleteItemPickupPointByPickupPointCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      DeleteItemPickupPointRequest deleteItemPickupPointRequest);

  @RequestLine(
      "GET /api/itemPickupPoint/getProductSkuListByBusinessPartnerAndPickupPointCode?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}&page={page}&size={size}&businessPartnerCode={businessPartnerCode}&pickupPointCode={pickupPointCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductSkuPickupPointResponseV2> getActiveProductsByPickupPointCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username, @Param("page") int page,
      @Param("size") int size, @Param("businessPartnerCode") String businessPartnerCode,
      @Param("pickupPointCode") String pickupPointCode);

  @RequestLine("GET /api/item/get-item-basic-details-by-product-sku?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetails(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku);

  @RequestLine(
      "POST /api/item/get-item-basic-details-by-item-skus?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}&inAllProducts={inAllProducts}&needProductData={needProductData}&needCategoryData={needCategoryData}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetailsByItemSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("inAllProducts") boolean inAllProducts,
      @Param("needProductData") boolean needProductData, @Param("needCategoryData") boolean needCategoryData,
      SimpleListStringRequest itemSkusList);

  @RequestLine("POST /api/listing/itemL5Details?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&productSku={productSku}&page={page}&size={size}&cncActivated={cncActivated}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemL5ListingResponse> getItemL5Details(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku, @Param("page") int page,
      @Param("size") int size, @Param("cncActivated") boolean cncActivated, SimpleListStringRequest l5Ids);

  @RequestLine("POST /api/listing/itemL5Details?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&page={page}&size={size}&cncActivated={cncActivated}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemL5ListingResponse> getItemL5DetailsByL5Ids(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page,
      @Param("size") int size, @Param("cncActivated") boolean cncActivated, SimpleListStringRequest l5Ids);

  @RequestLine(
      "POST api/product-v2/getMinAndMaxPriceForSkus?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PriceRangeResponse> getMinAndMaxPriceRange(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("merchantCode") String merchantCode, Set<String> listOfSkus);

  @RequestLine(
      "POST api/item/getBulkItemDetailByItemSkus?storeId={storeId}&channelId={channelId}&clientId={clientId}&"+
          "requestId={requestId}&username={username}&fetchBundleRecipe={fetchBundleRecipe}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ItemBasicDetailV2Response> getItemBasicDetailByItemSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("fetchBundleRecipe") boolean fetchBundleRecipe,
      SimpleListStringRequest simpleListStringRequest);

  @RequestLine("GET api/product-v2/{productSku}/basicProductInfo?storeId={storeId}&channelId={channelId}&clientId="
      + "{clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BasicProductResponse> getBasicProductInfo(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku);

  @RequestLine(
    "GET /api/product/{sellerCode}/{productCode}/sharedProduct?storeId={storeId" + "}&channelId"
      + "={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&findByProductSku={findByProductSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SimpleBooleanResponse> isSharedProduct(@Param("storeId") String storeId,
    @Param("channelId") String channelId, @Param("clientId") String clientId,
    @Param("requestId") String requestId, @Param("username") String username,
    @Param("sellerCode") String sellerCode, @Param("productCode") String productCode,
    @Param("findByProductSku") boolean findByProductSku);

  @RequestLine("POST /api/summary/filter/getProductSkuList?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}"
      + "&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProductSkuResponse> getProductSkuResponse(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("page") int page, @Param("size") int size,
      ProductSummaryRequest productSummaryRequest);

  @RequestLine("POST /api/product-v2/getProductBasicInfoDetails?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BulkDownloadProductBasicInfoResponse> getProductBasicInfoDetails(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String userName, ProductLevel3SummaryRequest request);

  @RequestLine(
      "GET /api/product/getProductDetailsByProductSku?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
          + "requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductL3Response> getProductDetailsByProductSku(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku);

  @RequestLine("GET api/product-v2/{productSku}/basicProductInfo?storeId={storeId}&channelId={channelId}&clientId="
      + "{clientId}&requestId={requestId}&username={username}&sharedProductInfoNeeded={sharedProductInfoNeeded}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductBasicResponse> getProductBasicInfo(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productSku") String productSku,
      @Param("sharedProductInfoNeeded") boolean sharedProductInfoNeeded);

  @RequestLine(
      "GET api/productList/generateProductScoreByProductSkuOrProductCode?storeId={storeId}&channelId={channelId}&clientId="
          + "{clientId}&requestId={requestId}&username={username}&productCode={productCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<L3VersionResponse> mapProductToItem(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("productCode") String productCode);
}
