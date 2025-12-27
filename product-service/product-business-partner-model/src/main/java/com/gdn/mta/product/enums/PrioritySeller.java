package com.gdn.mta.product.enums;

public enum PrioritySeller {
  NO_PRIORITY(0),
  PRIORITY_1(1),
  PRIORITY_2(2);

  private final int prioritySeller;

  PrioritySeller(int productType) {
    this.prioritySeller = productType;
  }

  public int getPrioritySeller() {
    return prioritySeller;
  }
}
