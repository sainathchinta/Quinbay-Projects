package com.gdn.partners.pbp.model.vo;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class ProductLv1IdxLv3IdVO {

  public ProductLv1IdxLv3IdVO(String productId, String gdnProductSku, Boolean preOrder) {
    this.productId = productId;
    this.gdnProductSku = gdnProductSku;
    this.preOrder = preOrder;
  }

  private String productId;
  private String productCode;
  private String gdnProductSku;
  private Boolean preOrder;
}
