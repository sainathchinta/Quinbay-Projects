package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OmniChannelSkuBasicDetailDTO implements Serializable {
  private String productCode;
  private String skuCode;
  private String omniChannelSku;
  private String productItemId;
  private String itemName;

  public OmniChannelSkuBasicDetailDTO(String productCode, String skuCode, String omniChannelSku, String productItemId) {
    this.productCode = productCode;
    this.skuCode = skuCode;
    this.omniChannelSku = omniChannelSku;
    this.productItemId = productItemId;
  }
}
