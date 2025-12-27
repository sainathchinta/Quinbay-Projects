package com.gdn.x.product.domain.event.enums;

public enum ProductType {

  REGULAR("Regular", 1),

  BIG_PRODUCT("Big Product", 2),

  BOPIS("BOPIS", 3);

  private String description;
  private int code;

  private ProductType(String description, int code) {
    this.description = description;
    this.code = code;
  }

  public int getCode() {
    return code;
  }

  public String getDescription() {
    return description;
  }

}
