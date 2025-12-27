package com.gdn.mta.bulk.dto;

public class ProductValueComparision {
  String oldValue;
  String newValue;

  public ProductValueComparision() {
    super();
  }

  public ProductValueComparision(String oldValue, String newValue) {
    this.oldValue = oldValue;
    this.newValue = newValue;
  }

  public String getOldValue() {
    return oldValue;
  }

  public void setOldValue(String oldValue) {
    this.oldValue = oldValue;
  }

  public String getNewValue() {
    return newValue;
  }

  public void setNewValue(String newValue) {
    this.newValue = newValue;
  }

  @Override public String toString() {
    final StringBuilder sb = new StringBuilder("ProductValueComparision{");
    sb.append("oldValue='").append(oldValue).append('\'');
    sb.append(", newValue='").append(newValue).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
