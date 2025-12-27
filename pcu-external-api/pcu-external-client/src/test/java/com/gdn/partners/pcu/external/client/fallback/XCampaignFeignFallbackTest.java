package com.gdn.partners.pcu.external.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.ProductCampaignAvailabilityRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.ProductCampaignAvailabilityResponse;

public class XCampaignFeignFallbackTest {

  private XCampaignFeignFallback xCampaignFeignFallback = new XCampaignFeignFallback();

  @Test
  public void getCampaignAvailabilityTest() {
    GdnRestSingleResponse<ProductCampaignAvailabilityResponse>
        response = this.xCampaignFeignFallback.getCampaignAvailability(new ProductCampaignAvailabilityRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getCampaignPriceInfoTest() {
    com.gdn.common.web.wrapper.response.GdnRestSingleResponse<CampaignPriceResponse> response =
        this.xCampaignFeignFallback.getCampaignPriceInfo(new CampaignPriceRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getCampaignPriceInfoV2Test() {
    com.gdn.common.web.wrapper.response.GdnRestSingleResponse<CampaignPriceResponse> response =
      this.xCampaignFeignFallback.getCampaignPriceInfoV2(new CampaignPriceRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getCampaignAvailabilityV2Test() {
    GdnRestSingleResponse<ProductCampaignAvailabilityResponse>
        response = this.xCampaignFeignFallback.getCampaignAvailabilityV2(new ProductCampaignAvailabilityRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }
}
