package com.gdn.mta.bulk;

public enum BulkUpdateChangeType {

  MERCHANT_SKU_CHANGED("MERCHANT_SKU_CHANGED"),
  PRICE_CHANGED("PRICE_CHANGED"),
  STOCK_CHANGED("STOCK_CHANGED"),
  PO_QUOTA_CHANGED("PO_QUOTA_CHANGED"),
  STATUS_CHANGED("STATUS_CHANGED"),
  CNC_CHANGED("CNC_CHANGED"),
  PICKUP_POINT_CHANGED_FOR_NON_MPP("PICKUP_POINT_CHANGED_FOR_NON_MPP"),
  ADD_PICKUP_POINT("ADD_PICKUP_POINT"),
  BFB_PRICE_CHANGED("BFB_PRICE_CHANGED"),
  BFB_MANAGED_CHANGED("BFB_MANAGED_CHANGED"),
  BFB_STATUS_CHANGED("BFB_STATUS_CHANGED"),
  SKIPPED("SKIPPED");


  private String statusValue;

  BulkUpdateChangeType(String statusValue) {
    this.statusValue = statusValue;

  }

  public String getStatusValue() {
    return statusValue;
  }
}
