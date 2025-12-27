package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class BundleItemWebResponse {
  private String itemSku;
  private String itemCode;
  private String itemName;
  private String mainImageUrl;
  private int quantity;
  private String productStatus;
  private String productSku;
  private String pdpUrl;
  private boolean sharedProduct;
}
