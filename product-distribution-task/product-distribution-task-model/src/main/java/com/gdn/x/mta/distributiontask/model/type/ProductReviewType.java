package com.gdn.x.mta.distributiontask.model.type;

import java.util.HashMap;
import java.util.Map;

public enum ProductReviewType {
  NEWLY_ADDED("0"),
  EDITED("1"),
  REVISED("2");

  private final String value;

  ProductReviewType(final String value) {
    this.value = value;
  }
  public String getValue() { return value; }

  private static final Map<String, String> MAP_BY_VALUE = new HashMap<>();

  static {
    for (ProductReviewType productReviewType: values()) {
      MAP_BY_VALUE.put(productReviewType.value, productReviewType.name());
    }
  }

  public static String getProductReviewTypeByValue(String value) {
    return MAP_BY_VALUE.get(value);
  }
}
