package com.gdn.x.product.rest.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class BundleItemResponse {
  private static final long serialVersionUID = -663517770553833436L;
  private String itemSku;
  private String itemCode;
  private String itemName;
  private String mainImageUrl;
  private int quantity;
  private String productStatus;
  private String productSku;
  private boolean sharedProduct;
}
