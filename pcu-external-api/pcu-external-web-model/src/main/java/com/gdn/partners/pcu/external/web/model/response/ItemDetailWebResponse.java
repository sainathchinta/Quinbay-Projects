package com.gdn.partners.pcu.external.web.model.response;

import java.util.ArrayList;
import java.util.List;

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
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemDetailWebResponse {
  private String itemName;
  private String itemSku;
  private String productName;
  private String productSku;
  private String merchantCode;
  private String itemCode;
  private String mainImageUrl;
  private String categoryCode;
  private String categoryName;
  private String pdpUrl;
  private boolean sharedProduct;
  private List<ItemAttributeWebResponse> itemAttributes = new ArrayList<>();
  private List<BundleChildRecipeWebResponse> bundleRecipeList = new ArrayList<>();


  public ItemDetailWebResponse(String itemName, String itemSku, String productName, String productSku) {
    this.itemName = itemName;
    this.itemSku = itemSku;
    this.productName = productName;
    this.productSku = productSku;
  }
}
