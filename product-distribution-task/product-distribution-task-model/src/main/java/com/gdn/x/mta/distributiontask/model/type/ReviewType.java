package com.gdn.x.mta.distributiontask.model.type;

import java.util.HashMap;
import java.util.Map;

public enum ReviewType {
  CONTENT(1),
  IMAGE(2),
  CONTENT_AND_IMAGE(3);

  private final int value;

  ReviewType(final int value) {
    this.value = value;
  }
  public int getValue() { return value; }

  private static final Map<Integer, String> MAP_BY_VALUE = new HashMap<>();

  static {
    for (ReviewType reviewType: values()) {
      MAP_BY_VALUE.put(reviewType.value, reviewType.name());
    }
  }

  public static String getReviewTypeByValue(int value) {
    return MAP_BY_VALUE.get(value);
  }
}
