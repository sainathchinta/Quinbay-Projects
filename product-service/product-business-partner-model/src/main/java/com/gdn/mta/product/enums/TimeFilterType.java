package com.gdn.mta.product.enums;

import java.util.HashMap;
import java.util.Map;

public enum  TimeFilterType {
  ALL("ALL"),
  TODAY("today"),
  YESTERDAY("yesterday"),
  TWO_DAYS_AGO("twoDaysAgo"),
  THREE_TO_FIVE_DAYS_AGO("threeToFiveDays"),
  FIVE_DAYS_AGO("moreThanFiveDaysAgo");

  public final String timeFilterType;

  TimeFilterType(String timeFilterType) {
    this.timeFilterType = timeFilterType;
  }


  private static final Map<String, TimeFilterType> lookUpMap = new HashMap<>();

  static {
    for (TimeFilterType timeFilter: TimeFilterType.values()) {
      lookUpMap.put(timeFilter.getTimeFilterType(), timeFilter);
    }
  }

  public String getTimeFilterType() {
    return timeFilterType;
  }

  public static TimeFilterType getTimeFilterTypeByValue(String timeFilterType) {
    return lookUpMap.get(timeFilterType);
  }
}
