package com.gdn.x.product.enums;

public enum SalesChannel {

  COMBINED_CHANNEL("B2C,B2B"),
  B2B_CHANNEL("B2B"),
  B2C_CHANNEL("B2C"),
  NO_CHANNEL("-");

  String name;

  SalesChannel(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }
}
