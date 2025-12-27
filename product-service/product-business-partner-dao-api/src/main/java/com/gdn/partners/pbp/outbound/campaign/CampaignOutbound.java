package com.gdn.partners.pbp.outbound.campaign;

import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignUpdateDiscountResponse;

public interface CampaignOutbound {

  /**
   * get campaign response v2
   * @param campaignPriceRequest
   * @return
   */
  CampaignPriceResponse getCampaignPriceInfoV2(CampaignPriceRequest campaignPriceRequest) throws Exception;

  /**
   * validate and update discount price in campaign
   * @param validation
   * @param campaignUpdateDiscountRequest
   * @return
   */
  CampaignUpdateDiscountResponse validateAndUpdateDiscountPrice(boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest) throws Exception;

  /**
   * Validate discount price
   * @param validation
   * @param campaignUpdateDiscountRequest
   * @return
   * @throws Exception
   */
  CampaignUpdateDiscountResponse validateDiscountPrice(boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest) throws Exception;
}
