package com.gdn.x.mta.distributiontask.model.enums;

import java.util.HashMap;
import java.util.Map;

public enum SellerBadge {
  NONE_MERCHANT(0),
  BRONZE_MERCHANT(1),
  SILVER_MERCHANT(2),
  GOLD_MERCHANT(3),
  DIAMOND_MERCHANT(4),
  OFFICIAL_STORES(5);

  private final int value;

  SellerBadge(final int value) {
    this.value = value;
  }

  public int getValue() {
    return value;
  }

  private static final Map<Integer, String> MAP_BY_VALUE = new HashMap<>();

  static {
    for (SellerBadge sellerBadge : values()) {
      MAP_BY_VALUE.put(sellerBadge.value, sellerBadge.name());
    }
  }

  public static String getSellerBadgeByValue(int value) {
    return MAP_BY_VALUE.get(value);
  }
}
