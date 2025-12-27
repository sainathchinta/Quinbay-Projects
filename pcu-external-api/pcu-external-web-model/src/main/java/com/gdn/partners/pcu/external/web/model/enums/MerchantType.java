package com.gdn.partners.pcu.external.web.model.enums;

public enum MerchantType {
  PURE_DELIVERY_SELLER("PURE_DELIVERY_SELLER", 1), CNC_SELLER("CNC_SELLER", 2), BFB_SELLER(
    "BFB_SELLER", 3), BFB_CNC_SELLER("BFB_CNC_SELLER", 4);

  private final String value;
  private final int type;


  private MerchantType(String value, int type) {
    this.value = value;
    this.type = type;
  }

  public String getValue() {
    return value;
  }
  public int getType() {
    return type;
  }
}
