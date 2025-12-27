package com.gdn.x.product.rest.web.model.response;

import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class Off2OnPriceResponseV2 extends BaseResponse {
  private String itemSku;
  private String pickupPointCode;
  private double offerPrice;
  private double listPrice;
  private boolean off2OnChannelActive;
  private Set<String> activePromoBundlings;
  private Set<String> sellerActivePromoBundlings;
}
