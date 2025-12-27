package com.gdn.x.product.enums;

public enum DescriptiveAttributeValueType {
  NONE("None"),
  SINGLE("Single"),
  MULTIPLE("Multiple"),
  PREDEFINED("Predefined");

  private final String description;

  private DescriptiveAttributeValueType(String description) {
    this.description = description;
  }

  public String getDescription() {
    return this.description;
  }
}
