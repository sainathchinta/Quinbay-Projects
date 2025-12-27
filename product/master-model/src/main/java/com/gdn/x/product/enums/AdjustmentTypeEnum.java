package com.gdn.x.product.enums;

public enum AdjustmentTypeEnum {
  BLIBLI("Blibli"),

  MERCHANT("Merchant");

  private String description;

  private AdjustmentTypeEnum(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }
}
