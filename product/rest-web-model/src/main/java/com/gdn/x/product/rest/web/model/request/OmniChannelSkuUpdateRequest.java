package com.gdn.x.product.rest.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class OmniChannelSkuUpdateRequest {
  private String itemCode;
  private String omniChannelSku;
}
