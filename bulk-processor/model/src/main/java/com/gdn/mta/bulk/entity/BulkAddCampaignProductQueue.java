package com.gdn.mta.bulk.entity;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class BulkAddCampaignProductQueue extends BulkUpdateQueue implements Serializable {

  private static final long serialVersionUID = 4495431318080114182L;

  private CampaignItemSummaryRequest campaignItemSummaryRequest;
  private String campaignCode;
  private String merchantName;
  private double minDiscount;
  private double maxDiscount;
  private double minFinalPrice;
  private int minQuota;
  private boolean filterOutInactiveCn;

  public BulkAddCampaignProductQueue(CampaignItemSummaryRequest campaignItemSummaryRequest,
      String campaignCode, String merchantName, double maxDiscount, double minDiscount, double minFinalPrice) {
    this.campaignItemSummaryRequest = campaignItemSummaryRequest;
    this.campaignCode = campaignCode;
    this.merchantName = merchantName;
    this.minDiscount = minDiscount;
    this.maxDiscount = maxDiscount;
    this.minFinalPrice = minFinalPrice;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkAddCampaignProductQueue{");
    sb.append("campaignItemSummaryRequest='").append(campaignItemSummaryRequest).append('\'');
    sb.append(", campaignCode='").append(campaignCode).append('\'');
    sb.append(", merchantName='").append(merchantName).append('\'');
    sb.append(", minDiscount=").append(minDiscount).append('\'');
    sb.append(", maxDiscount=").append(maxDiscount).append('\'');
    sb.append(", minFinalPrice=").append(minFinalPrice).append('\'');
    sb.append(", minQuota=").append(minQuota).append('\'');
    sb.append(", filterOutInactiveCn=").append(filterOutInactiveCn);
    sb.append('}');
    return sb.toString();
  }

}
