package com.gdn.partners.pcu.external.web.model.response;

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
public class ProductL3ListingWebResponse {
  private String productSku;
  private String productName;
  private int variantCount;
  private String categoryName;
  private String imageUrl;
  private int l5Count;
  private boolean bundleProduct;
  private String pdpUrl;
  private ItemAndPickupPointBasicDetailWebResponse item;
}
