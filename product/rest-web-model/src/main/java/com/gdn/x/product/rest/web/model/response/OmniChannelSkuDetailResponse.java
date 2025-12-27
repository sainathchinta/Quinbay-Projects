package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class OmniChannelSkuDetailResponse {

  private String itemName;
  private String categoryCode;
  private String categoryName;
  private String skuCode;
  private String omniChannelSku;
  private String productSku;
  private String itemSku;
  private String halal;
  private String storage;
  private String mainImageUrl;
  private boolean distribution;
  private int dangerousLevel;
  private List<CogsResponse> pickupPointResponse = new ArrayList<>();
  private DistributionItemInfoResponse distributionItemInfo;
  private List<DimensionsAndUOMResponse> dimensionsAndUOMResponse = new ArrayList<>();
}