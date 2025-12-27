package com.gdn.mta.bulk.repository.campaign;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.XCampaignFeign;
import com.gdn.mta.bulk.models.CampaignProductUpdateDto;
import com.gdn.mta.bulk.models.CampaignProductUpdateRequest;
import com.gdn.mta.bulk.models.CampaignUpdateResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.campaign.dto.ItemDetailsDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.x.campaign.request.CampaignProductDetailRequest;
import com.gdn.x.campaign.response.CampaignProductDetailResponse;

@Repository
@Slf4j
public class CampaignRepositoryBean implements CampaignRepository {

  @Autowired
  private XCampaignFeign xCampaignFeign;

  @Override
  public List<CampaignProductDetailResponse> getCampaignProductDetailsV2(
    List<ItemDetailsDto> itemDetails, BulkAddCampaignProductQueue bulkAddCampaignProductQueue)
    throws Exception {
    try {
      int page = 0;
      CampaignProductDetailRequest campaignProductDetailRequest =
        new CampaignProductDetailRequest();
      campaignProductDetailRequest.setCampaignCode(bulkAddCampaignProductQueue.getCampaignCode());
      campaignProductDetailRequest.setItemDetails(itemDetails);

      GdnRestListResponse<CampaignProductDetailResponse> response = this.xCampaignFeign
        .getCampaignProductDetailsV2(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, page, itemDetails.size(),
          campaignProductDetailRequest);

      if (!response.isSuccess()) {
        log.error("Response is failed to get campaign v2 product details with error {} ",
          response.getErrorMessage());
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
      }
      return Optional.of(response).map(GdnRestListResponse::getContent)
        .orElse(Collections.emptyList());
    } catch (Exception e) {
      log.error("Error in failing to get campaign v2 product details with error ",e.getMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, e.getMessage());
    }
  }

  @Override
  public CampaignUpdateResponse updateCampaignFinalPriceAndQuota(List<CampaignProductUpdateDto> campaignProductUpdateDtoList,
      String campaignCode) {
    try {
      CampaignProductUpdateRequest campaignProductUpdateRequest = new CampaignProductUpdateRequest();
      campaignProductUpdateRequest.setCampaignCode(campaignCode);
      campaignProductUpdateRequest.setItemPPIdList(campaignProductUpdateDtoList);

      GdnRestSingleResponse<CampaignUpdateResponse> response = this.xCampaignFeign.updateCampaignFinalPriceAndQuota(
          Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
          campaignProductUpdateRequest);

      if (!response.isSuccess()) {
        log.error("Error while getting response from campaign for updateCampaignFinalPriceAndQuota with error {} ",
            response.getErrorMessage());
        throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
      }
      return response.getValue();
    } catch (Exception e) {
      log.error("Error while getting response from campaign for updateCampaignFinalPriceAndQuota with error : {}",
          e.getMessage(), e);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, e.getMessage());
    }
  }
}
