package com.gdn.x.mta.distributiontask.model.enums;

import java.util.HashMap;
import java.util.Map;

public enum SellerType {

  NON_TRUSTED_SELLER(0), TRUSTED_SELLER(1);

  private final int value;

  SellerType(final int value) {
    this.value = value;
  }

  public int getValue() {
    return value;
  }

  private static final Map<Integer, String> MAP_BY_VALUE = new HashMap<>();

  static {
    for (SellerType sellerType : values()) {
      MAP_BY_VALUE.put(sellerType.value, sellerType.name());
    }
  }

  public static String getSellerTypeByValue(int value) {
    return MAP_BY_VALUE.get(value);
  }
}
