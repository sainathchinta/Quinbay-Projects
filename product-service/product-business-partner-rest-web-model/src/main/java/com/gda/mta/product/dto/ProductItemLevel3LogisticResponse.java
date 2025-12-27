package com.gda.mta.product.dto;

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
public class ProductItemLevel3LogisticResponse {

  private String logisticProductCode;
  private String logisticProductName;
  private boolean selected;
  private boolean requiredLongLat;
  private String highlightedInformation;

}
