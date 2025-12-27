package com.gdn.mta.bulk.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessAddCampaignProductRequest extends BulkProcessUpdateRequest
    implements Serializable {

  private static final long serialVersionUID = 391684628261432917L;

  private CampaignItemSummaryRequest campaignItemSummaryRequest;
  private String campaignCode;
  private String merchantName;
  private double minDiscount;
  private double maxDiscount;
  private double minFinalPrice;
  private int minQuota;

  public BulkProcessAddCampaignProductRequest(CampaignItemSummaryRequest campaignItemSummaryRequest,
      String campaignCode, String merchantName, double minDiscount, double maxDiscount, double minFinalPrice) {
    this.campaignItemSummaryRequest = campaignItemSummaryRequest;
    this.campaignCode = campaignCode;
    this.merchantName = merchantName;
    this.minDiscount = minDiscount;
    this.maxDiscount = maxDiscount;
    this.minFinalPrice = minFinalPrice;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkProcessAddCampaignProductRequest{");
    sb.append("campaignItemSummaryRequest='").append(campaignItemSummaryRequest).append('\'');
    sb.append(", campaignCode='").append(campaignCode).append('\'');
    sb.append(", merchantName='").append(merchantName).append('\'');
    sb.append(", minDiscount=").append(minDiscount).append('\'');
    sb.append(", maxDiscount=").append(maxDiscount).append('\'');
    sb.append(", minFinalPrice=").append(minFinalPrice).append('\'');
    sb.append(", minQuota=").append(minQuota);
    sb.append('}');
    return sb.toString();
  }

}
