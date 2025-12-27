package com.gdn.mta.bulk;

public enum MerchantStatusType {
  PURE_DELIVERY(1, 1),
  DELIVERY_AND_CNC(2, 2),
  BFB(3, 4),
  BFB_AND_CNC(4, 5);

  MerchantStatusType(int type, int incrementBy) {
    this.type = type;
    this.incrementBy = incrementBy;
  }

  private final int type;
  private final int incrementBy;

  public int getType() {
    return type;
  }

  public int getIncrementBy() {
    return incrementBy;
  }

}
