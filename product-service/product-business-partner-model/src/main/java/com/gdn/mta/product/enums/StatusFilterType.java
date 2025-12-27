package com.gdn.mta.product.enums;

import java.util.HashMap;
import java.util.Map;

public enum StatusFilterType {
  ALL("ALL"),
  ASSIGNED("assigned"),
  UNASSIGNED("unassigned"),
  REVISED("revised"),
  BRAND_APPROVED("brandApproved"),
  BRAND_NOT_APPROVED("brandNotApproved");

  private final String statusFilterType;

  private static final Map<String, StatusFilterType> lookUpMap = new HashMap<>();

  static {
    for (StatusFilterType statusFilter: StatusFilterType.values()) {
      lookUpMap.put(statusFilter.getStatusFilterType(), statusFilter);
    }
  }

  StatusFilterType(String statusFilterType) {
    this.statusFilterType = statusFilterType;
  }

  public String getStatusFilterType() {
    return statusFilterType;
  }

  public static StatusFilterType getStatusFilterTypeByValue(String statusFilterType) {
    return lookUpMap.get(statusFilterType);
  }
}
