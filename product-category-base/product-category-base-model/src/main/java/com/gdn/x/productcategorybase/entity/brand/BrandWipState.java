package com.gdn.x.productcategorybase.entity.brand;

public enum BrandWipState {
  DRAFT("In review"),
  APPROVED("Approved"),
  REJECTED("Rejected"),
  UPDATED("Updated"),
  DELETED("Deleted");

  BrandWipState(String description) {
    this.description = description;
  }

  private String description;

  public String getDescription() {
    return description;
  }
}