package com.gdn.x.product.model.entity;

public enum AdjustmentType {

  BLIBLI("blibli"),
  MERCHANT("merchant");

  private String description;

  private AdjustmentType(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }
}
