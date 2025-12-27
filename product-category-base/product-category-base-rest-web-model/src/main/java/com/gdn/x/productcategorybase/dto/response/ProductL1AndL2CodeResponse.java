package com.gdn.x.productcategorybase.dto.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL1AndL2CodeResponse {
  private String productCode;
  private String skuCode;
  private String omniChannelSku;
  private String itemName;
  private String productItemId;
  private String itemSku;
  private DistributionItemInfoResponse distributionItemInfoResponse;
  private List<DimensionsAndUomResponse> dimensionsAndUomResponse = new ArrayList<>();

  public ProductL1AndL2CodeResponse(String productCode, String skuCode, String omniChannelSku, String itemName) {
    this.productCode = productCode;
    this.skuCode = skuCode;
    this.omniChannelSku = omniChannelSku;
    this.itemName = itemName;
  }

  public ProductL1AndL2CodeResponse(String productCode, String skuCode, String omniChannelSku, String itemName,
      String productItemId) {
    this.productCode = productCode;
    this.skuCode = skuCode;
    this.omniChannelSku = omniChannelSku;
    this.itemName = itemName;
    this.productItemId = productItemId;
  }
}
