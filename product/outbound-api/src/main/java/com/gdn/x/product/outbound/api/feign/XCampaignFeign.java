package com.gdn.x.product.outbound.api.feign;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignUpdateDiscountResponse;

import com.gdn.x.product.outbound.api.feign.config.CampaignFeignProperties;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;



@FeignClient(name = "campaignClient", url = "${api.xcampaign.host}", configuration =
  CampaignFeignProperties.class)
public interface XCampaignFeign {

  @PostMapping(value = "/api/product/campaign-price", consumes = MediaType.APPLICATION_JSON_VALUE
    , produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CampaignPriceResponse> getCampaignPriceInfo(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestBody CampaignPriceRequest campaignPriceRequest);

  @PostMapping(value = "/api/v2/product/campaign-price", consumes = MediaType.APPLICATION_JSON_VALUE
    , produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CampaignPriceResponse> getCampaignPriceInfoV2(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestBody CampaignPriceRequest campaignPriceRequest);

  @PostMapping("/api/product/validate-update-discount-price")
  GdnRestSingleResponse<CampaignUpdateDiscountResponse> updateCampaignDiscount(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("validation") boolean validation,
    @RequestBody CampaignUpdateDiscountRequest campaignUpdateDiscountRequest);

  @PostMapping("/api/v2/product/validate-update-discount-price")
  GdnRestSingleResponse<CampaignUpdateDiscountResponse> updateCampaignDiscountV2(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("validation") boolean validation,
    @RequestBody CampaignUpdateDiscountRequest campaignUpdateDiscountRequest);
}
