package com.gdn.partners.pcu.internal.web.model.request;

import java.util.HashMap;
import java.util.Map;

public enum TimeFilterWebType {
  ALL("ALL"),
  TODAY("today"),
  YESTERDAY("yesterday"),
  TWO_DAYS_AGO("twoDaysAgo"),
  THREE_TO_FIVE_DAYS_AGO("threeToFiveDays"),
  FIVE_DAYS_AGO("moreThanFiveDaysAgo");

  public final String timeFilterType;
  private static final Map<String, TimeFilterWebType> lookUpMap = new HashMap();

  private TimeFilterWebType(String timeFilterType) {
    this.timeFilterType = timeFilterType;
  }

  public String getTimeFilterType() {
    return this.timeFilterType;
  }

  public static TimeFilterWebType getTimeFilterTypeByValue(String timeFilterType) {
    return (TimeFilterWebType) lookUpMap.get(timeFilterType);
  }

  static {
    TimeFilterWebType[] var0 = values();
    int var1 = var0.length;

    for(int var2 = 0; var2 < var1; ++var2) {
      TimeFilterWebType timeFilter = var0[var2];
      lookUpMap.put(timeFilter.getTimeFilterType(), timeFilter);
    }

  }
}
