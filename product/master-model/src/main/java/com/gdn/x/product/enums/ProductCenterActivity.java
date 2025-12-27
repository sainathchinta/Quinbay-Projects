package com.gdn.x.product.enums;

public enum ProductCenterActivity {

  COPY("Copy"),
  MOVE("Move"),
  DELETE("Delete"),
  ETD_NOTES_UPDATE("ETD Notes Update");

  private final String description;

  private ProductCenterActivity(String description) {
    this.description = description;
  }

  public String getDescription() {
    return this.description;
  }
}
