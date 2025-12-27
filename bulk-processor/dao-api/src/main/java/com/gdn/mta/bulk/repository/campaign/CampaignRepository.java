package com.gdn.mta.bulk.repository.campaign;

import java.util.List;

import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.models.CampaignProductUpdateDto;
import com.gdn.mta.bulk.models.CampaignUpdateResponse;
import com.gdn.x.campaign.dto.ItemDetailsDto;
import com.gdn.x.campaign.response.CampaignProductDetailResponse;

public interface CampaignRepository {

  /**
   * Get Campaign Product Details V2
   *
   * @param itemDetails
   * @param bulkAddCampaignProductQueue
   * @return List of Campaign Product Detail Response V2
   * @throws Exception
   */
  List<CampaignProductDetailResponse> getCampaignProductDetailsV2(List<ItemDetailsDto> itemDetails,
    BulkAddCampaignProductQueue bulkAddCampaignProductQueue) throws Exception;

  /**
   * Update campaign price and quota
   *
   * @param campaignProductUpdateDtoList
   * @param campaignCode
   * @return
   */
  CampaignUpdateResponse updateCampaignFinalPriceAndQuota(List<CampaignProductUpdateDto> campaignProductUpdateDtoList,
      String campaignCode);
}
