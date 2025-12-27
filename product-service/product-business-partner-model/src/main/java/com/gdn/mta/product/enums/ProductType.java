package com.gdn.mta.product.enums;

public enum ProductType {
  REGULAR(1), BIG_PRODUCT(2), BOPIS(3);

  private final Integer productType;

  ProductType(Integer productType) {
    this.productType = productType;
  }

  public Integer getProductType() {
    return productType;
  }
}
