package com.gdn.x.productcategorybase;

public enum RestrictedKeywordActivity {
  ADD("Add", "Keyword added"),
  DELETE("Delete", "Keyword deleted"),
  UI_VALIDATION_UPDATED("Ui validation updated", "Ui validation updated"),
  DS_VALIDATION_UPDATED("Ds validation updated", "Ds validation updated");

  private final String activity;
  private final String description;

  RestrictedKeywordActivity(String activity, String description) {
    this.activity = activity;
    this.description = description;
  }

  public String getDescription() {
    return description;
  }
}
