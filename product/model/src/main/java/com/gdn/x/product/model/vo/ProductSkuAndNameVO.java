package com.gdn.x.product.model.vo;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSkuAndNameVO implements Serializable {

  private static final long serialVersionUID = 4968276423762037703L;

  private String productSku;

  private String productName;

  public ProductSkuAndNameVO() {}

  public ProductSkuAndNameVO(String productSku, String productName) {
    this.productSku = productSku;
    this.productName = productName;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getProductName() {
    return productName;
  }

  public String getProductSku() {
    return productSku;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  @Override
  public String toString() {
    return String.format("ProductSkuAndNameDTO [productSku=%s, productName=%s, toString()=%s]",
        productSku, productName, super.toString());
  }


}
