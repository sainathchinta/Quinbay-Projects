package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.models.CampaignProductUpdateRequest;
import com.gdn.mta.bulk.models.CampaignUpdateResponse;
import com.gdn.x.campaign.request.CampaignProductDetailRequest;
import com.gdn.x.campaign.request.CampaignProductRequest;
import com.gdn.x.campaign.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.request.ProductCampaignAvailabilityRequest;
import com.gdn.x.campaign.response.CampaignProductDetailResponse;
import com.gdn.x.campaign.response.CampaignUpdateDiscountResponse;
import com.gdn.x.campaign.response.FailedProductsResponse;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

import java.util.List;

public interface XCampaignFeign {

  @RequestLine("POST /api/product/campaign-availability?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductCampaignAvailabilityResponse> getProductCampaign(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest);

  @RequestLine("POST /api/v2/product/campaign-availability?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductCampaignAvailabilityResponse> getProductCampaignV2(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest);

  @RequestLine("POST /api/product/validate-update-discount-price?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&validation={validation}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CampaignUpdateDiscountResponse> updateCampaignDiscount(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("validation") boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest);

  @RequestLine("POST /api/v2/product/validate-update-discount-price?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&validation={validation}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CampaignUpdateDiscountResponse> updateCampaignDiscountV2(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("validation") boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest);

  @RequestLine("POST /api/v2/product/addProducts-new?"
    + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
    + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<FailedProductsResponse> addCampaignProductV2(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username,
    List<CampaignProductRequest> campaignProductRequestPartition);

  @RequestLine("POST /api/v2/product/getProductDetails?"
    + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
    + "&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<CampaignProductDetailResponse> getCampaignProductDetailsV2(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("page") int page, @Param("size") int size,
    CampaignProductDetailRequest campaignProductDetailRequest);

  @RequestLine("POST /api/v2/product/update-price-quota?"
    + "storeId={storeId}&channelId={channelId}&clientId={clientId}"
    + "&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CampaignUpdateResponse> updateCampaignFinalPriceAndQuota(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, CampaignProductUpdateRequest campaignProductUpdateRequest);
}
