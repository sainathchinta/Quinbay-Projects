package com.gdn.mta.product.enums;

public enum PreOrderType {
  DATE("Date"), DAYS("Days"), WEEK("WEEK");

  public final String preOrderType;

  PreOrderType(String preOrderType) {
    this.preOrderType = preOrderType;
  }

  public String getPreOrderType() {
    return preOrderType;
  }
}
