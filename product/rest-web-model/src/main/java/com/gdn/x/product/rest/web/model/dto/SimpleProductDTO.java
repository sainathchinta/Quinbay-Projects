package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleProductDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private String productSku;
  private String productCode;
  private boolean isSynchronized;


  public SimpleProductDTO() {}

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getProductCode() {
    return this.productCode;
  }

  public String getProductSku() {
    return this.productSku;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public boolean isSynchronized() {
    return isSynchronized;
  }

  public void setSynchronized(boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

  @Override
  public String toString() {
    return String.format("SimpleProductDTO [productSku=%s, productCode=%s, isSynchronized=%s, toString()=%s]",
        this.productSku, this.productCode, this.isSynchronized, super.toString());
  }

}
