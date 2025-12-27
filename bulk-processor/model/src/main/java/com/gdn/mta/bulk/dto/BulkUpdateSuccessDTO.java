package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkUpdateSuccessDTO {

  private String productName;
  private String productSku;
  private String pickupPointCode;
  private boolean discountAboveMax;
  private boolean itemCampaignMapped;
  private Boolean sameThreshold;
  private Boolean scheduleRemoval;
  private String changeType;
}
