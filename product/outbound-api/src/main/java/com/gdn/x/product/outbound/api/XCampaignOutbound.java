package com.gdn.x.product.outbound.api;

import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignUpdateDiscountResponse;

public interface XCampaignOutbound {

  /**
   * Get campaign price info
   *
   * @param storeId
   * @param campaignPriceRequest
   * @return
   * @throws Exception
   */
  CampaignPriceResponse getCampaignPriceInfo(String storeId,CampaignPriceRequest campaignPriceRequest) throws Exception;

  /**
   * validate and update discount price in campaign
   * @param validatation
   * @param campaignUpdateDiscountRequest
   * @return
   */
  CampaignUpdateDiscountResponse validateAndUpdateDiscountPrice(boolean validatation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest) throws Exception;
}
