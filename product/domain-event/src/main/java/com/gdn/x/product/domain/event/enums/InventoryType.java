package com.gdn.x.product.domain.event.enums;

public enum InventoryType {
  WITH_INVENTORY("With-inventory"),

  WITHOUT_INVENTORY("Without-inventory");

  private String description;

  private InventoryType(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }
}
