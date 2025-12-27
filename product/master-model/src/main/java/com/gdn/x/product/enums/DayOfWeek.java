package com.gdn.x.product.enums;

import java.util.Arrays;
import java.util.List;

public enum DayOfWeek {
  MONDAY(1), TUESDAY(2), WEDNESDAY(3), THURSDAY(4), FRIDAY(5), SATURDAY(6), SUNDAY(7);

  private final int value;

  public static final List<DayOfWeek> WORKING_DAYS =
      Arrays.asList(DayOfWeek.MONDAY, DayOfWeek.TUESDAY, DayOfWeek.WEDNESDAY, DayOfWeek.THURSDAY, DayOfWeek.FRIDAY);

  private DayOfWeek(int value) {
    this.value = value;
  }

  public int getValue() {
    return value;
  }

  public static boolean isWorkingDay(DayOfWeek dayOfWeek) {
    return WORKING_DAYS.contains(dayOfWeek);
  }
}
