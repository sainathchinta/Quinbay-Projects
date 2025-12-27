package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemAndPickupPointBasicDetailWebResponse {
  private String itemSku;
  private String itemCode;
  private String itemName;
  private String pickupPointCode;
  private String mainImageUrl;
}
