package com.gdn.x.product.rest.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemAndPickupPointBasicDetailResponse {
  private String itemSku;
  private String itemCode;
  private String itemName;
  private String pickupPointCode;
  private String mainImageUrl;
}
