package com.gdn.mta.bulk;

public enum AllowedQRGenerationType {
  STORE("STORE"),
  PRODUCT("PRODUCT"),
  ITEM("ITEM"),
  ITEM_PICKUP_POINT("ITEM_PICKUP_POINT"),
  ADD_TO_BAG("ADD_TO_BAG"),
  ALL_PRODUCTS("ALL_PRODUCTS");

  private String value;

  AllowedQRGenerationType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
}
