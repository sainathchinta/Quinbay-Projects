package com.gdn.mta.product.valueobject;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3SummaryFilterDetails {
  private String businessPartnerCode;
  private String productSku;
  private String itemSku;
  private boolean isNeedCorrection = false;
  private String productCode;
}
