package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkAddCampaignProductDTO extends BulkUpdateProcessDTO implements Serializable {

  private static final long serialVersionUID = -8361071284012621985L;
  private CampaignItemSummaryRequest campaignItemSummaryRequest;
  private String campaignCode;
  private String merchantName;
  private double maxDiscount;
  private double minDiscount;
  private double minFinalPrice;
  private int minQuota;
  private boolean filterOutInactiveCn;

  public BulkAddCampaignProductDTO(CampaignItemSummaryRequest campaignItemSummaryRequest,
      String campaignCode, String merchantName, Double maxDiscount, Double minDiscount, Double minFinalPrice) {
    this.campaignItemSummaryRequest = campaignItemSummaryRequest;
    this.campaignCode = campaignCode;
    this.merchantName = merchantName;
    this.maxDiscount = maxDiscount;
    this.minDiscount = minDiscount;
    this.minFinalPrice =minFinalPrice;
  }


  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkAddCampaignProductDTO{");
    sb.append("campaignItemSummaryRequest='").append(campaignItemSummaryRequest).append('\'');
    sb.append(", campaignCode='").append(campaignCode).append('\'');
    sb.append(", merchantName='").append(merchantName).append('\'');
    sb.append(", maxDiscount=").append(maxDiscount).append('\'');
    sb.append(", minDiscount=").append(minDiscount).append('\'');
    sb.append(", minFinalPrice=").append(minFinalPrice).append('\'');
    sb.append(", minQuota=").append(minQuota).append('\'');
    sb.append(", filterOutInactiveCn=").append(filterOutInactiveCn);
    sb.append('}');
    return sb.toString();
  }

}
