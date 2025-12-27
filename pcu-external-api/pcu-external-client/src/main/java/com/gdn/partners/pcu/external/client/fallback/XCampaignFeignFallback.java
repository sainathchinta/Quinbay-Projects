package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.external.client.feign.XCampaignFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.campaign.request.ProductCampaignAvailabilityRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;

@Component
public class XCampaignFeignFallback implements XCampaignFeign {
  @Override
  public GdnRestSingleResponse<ProductCampaignAvailabilityResponse> getCampaignAvailability(
      ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest) {
    return new GdnRestSingleResponse(
        ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public com.gdn.common.web.wrapper.response.GdnRestSingleResponse<CampaignPriceResponse> getCampaignPriceInfo(
    CampaignPriceRequest campaignPriceRequest) {
    return new com.gdn.common.web.wrapper.response.GdnRestSingleResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<CampaignPriceResponse> getCampaignPriceInfoV2(
    CampaignPriceRequest campaignPriceRequest) {
    return new GdnRestSingleResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProductCampaignAvailabilityResponse> getCampaignAvailabilityV2(
      ProductCampaignAvailabilityRequest productCampaignAvailabilityRequest) {
    return new GdnRestSingleResponse(
        ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}
