package com.gdn.x.productcategorybase.util;

import java.util.Calendar;

import org.springframework.stereotype.Component;

@Component
public class CalendarFactory implements ICalendarFactory {
  @Override
  public Calendar getInstance() {
    return Calendar.getInstance();
  }
}
