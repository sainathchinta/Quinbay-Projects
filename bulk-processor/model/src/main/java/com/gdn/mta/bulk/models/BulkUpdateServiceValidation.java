package com.gdn.mta.bulk.models;

import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.x.campaign.response.CampaignProductDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import lombok.Builder;
import lombok.Data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by tonny.kurniawan on 11/16/2018
 */
@Data
@Builder
public class BulkUpdateServiceValidation {

  private List<Map<String, String>> tempCleanDataList = new ArrayList<>();
  private BulkAddCampaignProductQueue bulkAddCampaignProductQueue;
  private Map<String, CampaignProductDetailResponse> campaignProductDetailResponseMap = new HashMap<>();
  private Map<String, ProductLevel3SummaryResponse> productLevel3SummaryResponseMap= new HashMap<>();
  private StringBuilder tempErrorMessage;
  private Map<String, ItemSummaryListResponse> itemSummaryMap = new HashMap<>();

}
