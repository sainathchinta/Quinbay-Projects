package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.factory.XCampaignFeignFallbackFactory;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.ProductCampaignAvailabilityRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@FeignClient(name = "xCampaignFeign", url = "${service.xcampaign.endpoint}",
             fallbackFactory = XCampaignFeignFallbackFactory.class)
public interface XCampaignFeign {

  @RequestMapping(value = "/api/product/campaign-availability", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductCampaignAvailabilityResponse> getCampaignAvailability(
      @RequestBody ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest);

  @RequestMapping(value = "/api/v2/product/campaign-availability", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProductCampaignAvailabilityResponse> getCampaignAvailabilityV2(
      @RequestBody ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest);

  @RequestMapping(value = "/api/product/campaign-price", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CampaignPriceResponse> getCampaignPriceInfo(
    @RequestBody CampaignPriceRequest campaignPriceRequest);

  @RequestMapping(value = "/api/v2/product/campaign-price", method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<CampaignPriceResponse> getCampaignPriceInfoV2(
    @RequestBody CampaignPriceRequest campaignPriceRequest);
}
