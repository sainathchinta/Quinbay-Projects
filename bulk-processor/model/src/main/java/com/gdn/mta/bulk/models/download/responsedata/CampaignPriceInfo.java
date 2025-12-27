package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CampaignPriceInfo implements Serializable {
  private static final long serialVersionUID = -5953992804677946825L;

  private boolean registeredInCampaign;
  private boolean lockPriceUpdate;
  private double minAllowedPrice;
  private double maxAllowedPrice;
  private String campaignCode;
  private double minCampaignPrice;
  private double maxCampaignPrice;
  private boolean lockCampaignPriceUpdate;
  private double campaignPrice;
  private String campaignName;
}
