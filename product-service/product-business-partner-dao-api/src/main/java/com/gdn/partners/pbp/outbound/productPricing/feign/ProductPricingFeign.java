package com.gdn.partners.pbp.outbound.productPricing.feign;

import java.util.List;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.dto.promo.request.FreeSampleParticipationResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.request.PromoBundlingPricingDetailRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoBundlingPricingSummaryRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingPricingDetailResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSummaryResponse;
import com.gdn.partners.product.pricing.web.model.request.BulkActivateDeactivateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceBulkUpdateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.BulkActivateDeactivateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceBulkUpdateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

/**
 * @author nitinmathew - created on 06/02/2020
 **/
public interface ProductPricingFeign {

  @RequestLine("POST /api/promo-bundling/detail/{promoBundlingCode}?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PromoBundlingPricingDetailResponse> getPromoBundlingDetail(
      @Param("storeId") String storeId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId,
      @Param("requestId") String requestId,
      @Param("username") String username,
      @Param("promoBundlingCode") String promoBundlingCode,
      @Param("page") int page,
      @Param("size") int size,
      PromoBundlingPricingDetailRequest request);

  @RequestLine("POST /api/v2/promo-bundling/detail/{promoBundlingCode}?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PromoBundlingPricingDetailResponse> getPromoBundlingDetailV2(
      @Param("storeId") String storeId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId,
      @Param("requestId") String requestId,
      @Param("username") String username,
      @Param("promoBundlingCode") String promoBundlingCode,
      @Param("page") int page,
      @Param("size") int size,
      PromoBundlingPricingDetailRequest request);

  @RequestLine("POST /api/v2/promo-bundling/list/filter?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page}&size={size}&sortBy={sortBy}&sortType={sortType}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PromoBundlingSummaryResponse> filterPromoBundlingV2(
      @Param("storeId") String storeId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId,
      @Param("requestId") String requestId,
      @Param("username") String username,
      @Param("page") int page,
      @Param("size") int size,
      @Param("sortBy") String sortBy,
      @Param("sortType") String sortType,
      PromoBundlingPricingSummaryRequest request);

  @RequestLine("GET /api/free-promo/free-sample-participation-check/{productSku}?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&productSku={productSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<FreeSampleParticipationResponse> freeSampleParticipationCheck(
      @Param("storeId") String storeId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId,
      @Param("requestId") String requestId,
      @Param("username") String username,
      @Param("productSku") String productSku);

  @RequestLine("POST /api/v2/wholesale-price/sku/detail/list?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<WholesalePriceSkuResponse> getWholesalePriceListDetailV2(
      @Param("storeId") String storeId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId,
      @Param("requestId") String requestId,
      @Param("username") String username,
      WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest);

  @RequestLine("PUT /api/wholesale-price/sku/bulk-activate-deactivate?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BulkActivateDeactivateResponse> bulkActivateOrDeactivateSku(
      @Param("storeId") String storeId,
      @Param("requestId") String requestId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId,
      @Param("username") String username,
      List<BulkActivateDeactivateRequest> bulkActivateDeactivateRequests);

  @RequestLine("PUT /api/v2/wholesale-price/sku/bulk-activate-deactivate?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<BulkActivateDeactivateResponse> bulkActivateOrDeactivateSkuV2(
      @Param("storeId") String storeId,
      @Param("requestId") String requestId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId,
      @Param("username") String username,
      List<BulkActivateDeactivateRequest> bulkActivateDeactivateRequests);

  @RequestLine("PUT /api/wholesale-price/bulk-update?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> bulkUpdate(
      @Param("storeId") String storeId,
      @Param("requestId") String requestId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId,
      @Param("username") String username,
      @Param("merchantCode") String merchantCode,
      List<WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequests);

  @RequestLine("PUT /api/v2/wholesale-price/bulk-update?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> bulkUpdateV2(
      @Param("storeId") String storeId,
      @Param("requestId") String requestId,
      @Param("channelId") String channelId,
      @Param("clientId") String clientId,
      @Param("username") String username,
      @Param("merchantCode") String merchantCode,
      List<WholesalePriceBulkUpdateRequest> wholesalePriceBulkUpdateRequests);
}
