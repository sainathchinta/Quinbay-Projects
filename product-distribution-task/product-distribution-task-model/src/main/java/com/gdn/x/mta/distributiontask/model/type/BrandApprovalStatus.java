package com.gdn.x.mta.distributiontask.model.type;

import java.util.HashMap;
import java.util.Map;

public enum BrandApprovalStatus {

  DRAFT("0"),
  APPROVED("1"),
  REJECTED("2");

  private final String value;

  BrandApprovalStatus(final String value) {
    this.value = value;
  }
  public String getValue() { return value; }

  private static final Map<String, String> MAP_BY_VALUE = new HashMap<>();

  static {
    for (BrandApprovalStatus brandApprovalStatus: values()) {
      MAP_BY_VALUE.put(brandApprovalStatus.value, brandApprovalStatus.name());
    }
  }

  public static String getBrandApprovalStatusByValue(String value) {
    return MAP_BY_VALUE.get(value);
  }
}
