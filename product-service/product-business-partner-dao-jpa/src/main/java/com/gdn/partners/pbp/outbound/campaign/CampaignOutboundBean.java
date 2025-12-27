package com.gdn.partners.pbp.outbound.campaign;

import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.campaign.rest.web.model.dto.ItemInfoStatusDto;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignUpdateDiscountResponse;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class CampaignOutboundBean implements CampaignOutbound{

  @Autowired
  private XCampaignFeign xCampaignFeign;


  @Override
  public CampaignPriceResponse getCampaignPriceInfoV2(CampaignPriceRequest campaignPriceRequest) throws Exception {
    MandatoryRequestParam param = this.generateMandatoryRequestParam();
    GdnRestSingleResponse<CampaignPriceResponse> campaignPriceResponseGdnRestSingleResponse =
        xCampaignFeign.getCampaignPriceInfoV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
            param.getRequestId(), param.getUsername(), campaignPriceRequest);
    if (!campaignPriceResponseGdnRestSingleResponse.isSuccess()) {
      log.warn("Error while getting  campaign price with request : {} :: error message {} :: error code",
          campaignPriceRequest, campaignPriceResponseGdnRestSingleResponse.getErrorMessage(),
          campaignPriceResponseGdnRestSingleResponse.getErrorCode());
    }
    return campaignPriceResponseGdnRestSingleResponse.getValue();
  }

  @Override
  public CampaignUpdateDiscountResponse validateAndUpdateDiscountPrice(boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest) throws Exception {
    MandatoryRequestParam param = this.generateMandatoryRequestParam();
    log.info("Update campaign discount price , campaignUpdateDiscountRequest : {} ", campaignUpdateDiscountRequest);
    GdnRestSingleResponse<CampaignUpdateDiscountResponse> discountResponseGdnRestSingleResponse = null;
    discountResponseGdnRestSingleResponse =
        xCampaignFeign.updateCampaignDiscountV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
            param.getRequestId(), param.getUsername(), validation, campaignUpdateDiscountRequest);
    if (!discountResponseGdnRestSingleResponse.isSuccess()) {
      log.error("Exception caught on updating the discount price : campaignUpdateDiscountRequest " + "{} ",
          campaignUpdateDiscountRequest);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          discountResponseGdnRestSingleResponse.getErrorMessage());
    }
    return discountResponseGdnRestSingleResponse.getValue();
  }

  @Override
  public CampaignUpdateDiscountResponse validateDiscountPrice(boolean validation,
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest) throws Exception {
    MandatoryRequestParam param = this.generateMandatoryRequestParam();
    log.info("Validate campaign discount price , campaignUpdateDiscountRequest : {} ", campaignUpdateDiscountRequest);
    GdnRestSingleResponse<CampaignUpdateDiscountResponse> discountResponseGdnRestSingleResponse = null;
    discountResponseGdnRestSingleResponse =
        xCampaignFeign.validateDiscountPriceV2(param.getStoreId(), param.getChannelId(), param.getClientId(),
            param.getRequestId(), param.getUsername(), validation, campaignUpdateDiscountRequest);
    setItemSkuStatusMap(discountResponseGdnRestSingleResponse);
    if (!discountResponseGdnRestSingleResponse.isSuccess()) {
      log.error("Exception caught on validating the discount price : campaignUpdateDiscountRequest {}",
          campaignUpdateDiscountRequest);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
          discountResponseGdnRestSingleResponse.getErrorMessage());
    }
    return discountResponseGdnRestSingleResponse.getValue();
  }

  private void setItemSkuStatusMap(
      GdnRestSingleResponse<CampaignUpdateDiscountResponse> discountResponseGdnRestSingleResponse) {
      if (Objects.nonNull(discountResponseGdnRestSingleResponse.getValue()) && CollectionUtils
          .isNotEmpty(discountResponseGdnRestSingleResponse.getValue().getItemInfoStatus()) && MapUtils
          .isEmpty(discountResponseGdnRestSingleResponse.getValue().getItemSkuStatusMap())) {
        Map<String, String> itemSkuStatusMap =
            discountResponseGdnRestSingleResponse.getValue().getItemInfoStatus().stream()
                .collect(Collectors.toMap(ItemInfoStatusDto::getItemSku, ItemInfoStatusDto::getStatus, (a, b) -> a));
        discountResponseGdnRestSingleResponse.getValue().setItemSkuStatusMap(itemSkuStatusMap);
      }
  }

  private MandatoryRequestParam generateMandatoryRequestParam() throws Exception {
    return MandatoryRequestParam.generateMandatoryRequestParam(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        GdnMandatoryRequestParameterUtil.getAuthenticator());
  }

}
