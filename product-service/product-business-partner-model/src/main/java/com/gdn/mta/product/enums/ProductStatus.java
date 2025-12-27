package com.gdn.mta.product.enums;

public enum ProductStatus {

  ACTIVE("ACTIVE"),
  CREATED("CREATED"),
  REJECTED("REJECTED"),
  NEED_CORRECTION("NEED_CORRECTION");
  private final String productStatus;

  ProductStatus(String productStatus) {
    this.productStatus = productStatus;
  }

  public String getProductStatus() {
    return productStatus;
  }
}
