package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.Map;

import com.gdn.common.base.GdnObjects;

public class AddProductAndItemsResponseVo implements Serializable {
  private static final long serialVersionUID = 1L;

  private String merchantCode;
  private String productCode;
  private String productSku;
  private Map<String, String> mapOfItemSkuByItemCode;

  public AddProductAndItemsResponseVo() {}

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }

  public Map<String, String> getMapOfItemSkuByItemCode() {
    return this.mapOfItemSkuByItemCode;
  }

  public String getMerchantCode() {
    return this.merchantCode;
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

  public void setMapOfItemSkuByItemCode(Map<String, String> mapOfItemSkuByItemCode) {
    this.mapOfItemSkuByItemCode = mapOfItemSkuByItemCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  @Override
  public String toString() {
    return String
        .format(
            "AddProductAndItemsResponse [merchantCode=%s, productCode=%s, productSku=%s, mapOfItemSkuByItemCode=%s, toString()=%s]",
            this.merchantCode, this.productCode, this.productSku, this.mapOfItemSkuByItemCode,
            super.toString());
  }
}
