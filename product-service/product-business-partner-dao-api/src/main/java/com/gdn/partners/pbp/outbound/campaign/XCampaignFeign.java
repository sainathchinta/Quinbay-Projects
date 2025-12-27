package com.gdn.partners.pbp.outbound.campaign;


import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignUpdateDiscountResponse;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface XCampaignFeign {

  @RequestLine("POST /api/v2/product/campaign-price?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CampaignPriceResponse> getCampaignPriceInfoV2(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, CampaignPriceRequest campaignPriceRequest);

  @RequestLine("POST /api/product/validate-update-discount-price?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&validation={validation}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CampaignUpdateDiscountResponse> updateCampaignDiscount(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("validation") boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest);

  @RequestLine("POST /api/v2/product/validate-update-discount-price?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&validation={validation}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CampaignUpdateDiscountResponse> updateCampaignDiscountV2(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("validation") boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest);

  @RequestLine("POST /api/product/validate-discount-price?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&validation={validation}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CampaignUpdateDiscountResponse> validateDiscountPrice(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("validation") boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest);

  @RequestLine("POST /api/v2/product/validate-discount-price?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}&validation={validation}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<CampaignUpdateDiscountResponse> validateDiscountPriceV2(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("validation") boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest);
}
