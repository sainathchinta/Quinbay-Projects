package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IndicativePriceDetails {
  private String itemSku;
  private String masterItemSku;
  private double maxPrice;
  private double minPrice;
  private String competitorSourceName;
  private String competitorUrl;
  private double competitorPrice;
}
