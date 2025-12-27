package com.gdn.x.product.model.vo;

import java.io.Serializable;

import com.gdn.common.base.GdnObjects;

public class SimpleProductRequestVo implements Serializable {

  private static final long serialVersionUID = 1L;

  private String productSku;
  private String productCode;
  private boolean isSynchronized;

  public SimpleProductRequestVo() {

  }

  public SimpleProductRequestVo(String productSku, String productCode, boolean isSynchronized) {
    super();
    this.productSku = productSku;
    this.productCode = productCode;
    this.isSynchronized = isSynchronized;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
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

  public boolean isSynchronized() {
    return this.isSynchronized;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setSynchronized(boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

}
