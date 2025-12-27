package com.gdn.x.product.enums;

public enum MasterDataTypeEnum {
  PRODUCT("product"),

  ITEM("item");

  private String description;

  private MasterDataTypeEnum(String description) {
    this.description = description;
  }

  public String getDescription() {
    return this.description;
  }
}
