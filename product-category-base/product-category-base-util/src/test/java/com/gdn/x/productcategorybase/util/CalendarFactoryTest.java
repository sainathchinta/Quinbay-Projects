package com.gdn.x.productcategorybase.util;


import org.junit.jupiter.api.Test;

public class CalendarFactoryTest {

  private CalendarFactory calendarFactory = new CalendarFactory();

  @Test
  public void getInstanceTest() throws Exception {
    calendarFactory.getInstance();
  }
}
