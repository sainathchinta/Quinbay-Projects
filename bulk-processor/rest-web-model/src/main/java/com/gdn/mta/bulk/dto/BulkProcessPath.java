package com.gdn.mta.bulk.dto;

public enum BulkProcessPath {
  SUSPENSION_PATH("suspension/"),
  CONFIGURATION("config/"),
  RECAT("recat/"),
  INTERNAL_UPLOAD("internal/"),
  BULK_REBATE("rebate/"), BULK_PRICE_PRODUCT_TYPE_TAGGING("productTypeTaggingUpdate/");
  private String value;

  private BulkProcessPath(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }

}
