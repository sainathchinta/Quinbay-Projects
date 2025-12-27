package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ProductItemDistributionInfoRequest {
  private String skuCode;
  private DistributionItemRequest distributionItemInfoRequest;
  private List<DimensionAndUomRequest> dimensionsAndUOMRequest;
  private String requestedItemName;

  public ProductItemDistributionInfoRequest(String skuCode, DistributionItemRequest distributionItemInfoRequest,
      List<DimensionAndUomRequest> dimensionsAndUOMRequest) {
    this.skuCode = skuCode;
    this.distributionItemInfoRequest = distributionItemInfoRequest;
    this.dimensionsAndUOMRequest = dimensionsAndUOMRequest;
  }
}
