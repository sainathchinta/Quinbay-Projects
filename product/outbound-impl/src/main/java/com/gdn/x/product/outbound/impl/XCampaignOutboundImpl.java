package com.gdn.x.product.outbound.impl;

import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.campaign.dto.ItemInfoStatusDto;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.response.CampaignUpdateDiscountResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.outbound.api.XCampaignOutbound;
import com.gdn.x.product.outbound.api.feign.XCampaignFeign;

import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class XCampaignOutboundImpl implements XCampaignOutbound {

  @Autowired
  private XCampaignFeign xCampaignFeign;


  @Override
  public CampaignPriceResponse getCampaignPriceInfo(String storeId, CampaignPriceRequest campaignPriceRequest) {
    GdnRestSingleResponse<CampaignPriceResponse> campaignPriceResponse;
    campaignPriceResponse =
        xCampaignFeign.getCampaignPriceInfoV2(storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest);
    if (!campaignPriceResponse.isSuccess()) {
      log.error("Error while getting  campaign price with request : {} :: error message {} : error code : {} ",
          campaignPriceRequest, campaignPriceResponse.getErrorMessage(), campaignPriceResponse.getErrorCode());
    }
    setItemSkuToPriceResponse(campaignPriceResponse);
    return campaignPriceResponse.getValue();
  }

  private void setItemSkuToPriceResponse(GdnRestSingleResponse<CampaignPriceResponse> campaignPriceResponse) {
    if (Objects.nonNull(campaignPriceResponse.getValue()) && CollectionUtils.isNotEmpty(
        campaignPriceResponse.getValue().getItemInfoToPriceResponse()) && MapUtils.isEmpty(
        campaignPriceResponse.getValue().getItemSkuToPriceResponse())) {
      Map<String, CampaignPriceSkuResponse> itemSkuToPriceResponse =
          campaignPriceResponse.getValue().getItemInfoToPriceResponse().stream().collect(
              Collectors.toMap(CampaignPriceSkuResponse::getItemSku,
                  campaignPriceSkuResponse -> campaignPriceSkuResponse, (a, b) -> a));
      campaignPriceResponse.getValue().setItemSkuToPriceResponse(itemSkuToPriceResponse);
    }
  }

  @Override
  public CampaignUpdateDiscountResponse validateAndUpdateDiscountPrice(boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest) {
    GdnRestSingleResponse<CampaignUpdateDiscountResponse> discountResponse = null;
    discountResponse = xCampaignFeign.updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, validation,
        campaignUpdateDiscountRequest);
    if (!discountResponse.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, discountResponse.getErrorMessage());
    }
    setItemSkuStatusMap(discountResponse);
    return discountResponse.getValue();
  }

  private void setItemSkuStatusMap(GdnRestSingleResponse<CampaignUpdateDiscountResponse> discountResponse) {
    if (Objects.nonNull(discountResponse.getValue()) && CollectionUtils.isNotEmpty(
        discountResponse.getValue().getItemInfoStatus()) && MapUtils.isEmpty(
        discountResponse.getValue().getItemSkuStatusMap())) {
      Map<String, String> itemSkuStatusMap = discountResponse.getValue().getItemInfoStatus().stream()
          .collect(Collectors.toMap(ItemInfoStatusDto::getItemSku, ItemInfoStatusDto::getStatus, (a, b) -> a));
      discountResponse.getValue().setItemSkuStatusMap(itemSkuStatusMap);
    }
  }
}
