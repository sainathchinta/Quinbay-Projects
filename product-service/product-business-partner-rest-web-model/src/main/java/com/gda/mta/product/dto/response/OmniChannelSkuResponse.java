package com.gda.mta.product.dto.response;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OmniChannelSkuResponse extends BaseResponse {
  private String productCode;
  private String skuCode;
  private String sellerSku;
  private String itemName;
  private String productItemId;
  private String productSku;
  private String itemSku;
  private String requestedItemName;

  public OmniChannelSkuResponse(String productCode, String skuCode, String sellerSku, String itemName,
      String productItemId, String productSku, String itemSku) {
    this.productCode = productCode;
    this.skuCode = skuCode;
    this.sellerSku = sellerSku;
    this.itemName = itemName;
    this.productItemId = productItemId;
    this.productSku = productSku;
    this.itemSku = itemSku;
  }
}
