package com.gdn.x.productcategorybase;

public enum BrandAuthorisationStatus {
  ACTIVE("ACTIVE"), INACTIVE("INACTIVE"), EXPIRED("EXPIRED"), NEAR_EXPIRY("NEAR_EXPIRY"), UPCOMING(
      "UPCOMING");
  private final String value;
  BrandAuthorisationStatus(String value) {
    this.value = value;
  }
  public String getValue() {
    return value;
  }
}
