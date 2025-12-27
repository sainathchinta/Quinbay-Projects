package com.gdn.mta.bulk.models;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CampaignProductUpdateDto {
  private String itemSku;
  private String itemPickupPointId;
  private double finalPrice;
  private double sellingPrice;
  private double originalSellingPrice;
  private int quota;
  private String categoryCode;
  private boolean isClearance;
}
