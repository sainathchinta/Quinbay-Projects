package com.gdn.partners.pbp.commons.util;

import java.lang.reflect.Constructor;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pbp.commons.constants.Constants;

public class CommonUtilsTest {
  public static final String DATE = "2021-09-01T03:31:00Z";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  Date date = new Date(1700000000000L);

  private Date startDate;
  private Date endDate;
  SimpleDateFormat dateFormat;

  private class AClassOfBean {
    private String prop1;
    private Integer prop2;

    public String getProp1() {
      return prop1;
    }

    public void setProp1(String prop1) {
      this.prop1 = prop1;
    }

    public Integer getProp2() {
      return prop2;
    }

    public void setProp2(Integer prop2) {
      this.prop2 = prop2;
    }

    @Override
    public String toString() {
      return CommonUtils.stringifyBean(this);
    }
  }

  @BeforeEach
  public void _setup() throws Exception {
    dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    startDate = dateFormat.parse("2021-09-01T09:01:02");
    endDate = dateFormat.parse("2021-09-02T09:01:02");
  }

  @AfterEach
  public void _teardown() {}

  @Test
  public void __constructor_test() throws Exception {
    Constructor<CommonUtils> constructor = CommonUtils.class.getDeclaredConstructor();
    constructor.setAccessible(true);
    CommonUtils logUtil = constructor.newInstance();
    Assertions.assertNotNull(logUtil);
  }

  @Test
  public void stringifyClass_Test() {
    CommonUtilsTest.AClassOfBean aBean = new CommonUtilsTest.AClassOfBean();
    Assertions.assertEquals(CommonUtils.stringifyBean(aBean), aBean.toString());
  }

  @Test
  public void stringifyClass_Test_null() {
    CommonUtilsTest.AClassOfBean aBean = null;
    Assertions.assertNull(CommonUtils.stringifyBean(aBean));
  }

  @Test
  public void toJakartaTimeZoneTest() throws Exception {
    String date = CommonUtils.toJakartaTimeZone(startDate);
    Assertions.assertNotNull(DATE, date);
  }

  @Test
  public void validateHistoryDateFilterLast3MonthsTest() {
    CommonUtils.validateHistoryDateFilterLastNMonths(new Date(), 3);
  }

  @Test
  public void validateHistoryDateFilterLast3MonthsAfterCurrentTest() throws Exception {
    Date date = new Date();
    Calendar c = Calendar.getInstance();
    c.setTime(date);
    c.add(Calendar.DATE, 1);
    date = c.getTime();
    Date finalDate = date;
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateHistoryDateFilterLastNMonths(finalDate, 3);
    });
  }

  @Test
  public void validateHistoryDateFilterLast3MonthsBefore3MonthTest() {
    Date date = new Date();
    Calendar c = Calendar.getInstance();
    c.setTime(date);
    c.add(Calendar.MONTH, -4);
    date = c.getTime();
    Date finalDate = date;
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateHistoryDateFilterLastNMonths(finalDate, 3);

    });
  }

  @Test
  public void validateStartDateIsBeforeEndDateTest() {
    CommonUtils.validateStartDateIsBeforeEndDate(startDate, endDate);
  }

  @Test
  public void validateStartDateIsBeforeEndDateExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateStartDateIsBeforeEndDate(endDate, startDate);
    });

  }

  @Test
  public void getItemSkuAndPickupPointKeyTest() {
    String key = CommonUtils.getItemSkuAndPickupPointKey(ITEM_SKU, PICKUP_POINT_CODE);
    Assertions.assertEquals(key, ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE);
  }

  @Test
  public void validateHistoryDateFilterLastNDaysTest() {
    CommonUtils.validateHistoryDateFilterLastNDays(new Date(), 30);
  }

  @Test
  public void validateHistoryDateFilterLast3DaysAfterCurrentTest() throws Exception {
    Date date = new Date();
    Calendar c = Calendar.getInstance();
    c.setTime(date);
    c.add(Calendar.DATE, 1);
    date = c.getTime();
    Date finalDate = date;
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateHistoryDateFilterLastNDays(finalDate, 30);
    });
  }

  @Test
  public void validateHistoryDateFilterLast3DaysBefore3MonthTest() {
    Date date = new Date();
    Calendar c = Calendar.getInstance();
    c.setTime(date);
    c.add(Calendar.MONTH, -4);
    date = c.getTime();
    Date finalDate = date;
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateHistoryDateFilterLastNDays(finalDate, 30);
    });
  }

  @Test
  public void validateHistoryDateNullDateTest() {
    CommonUtils.validateHistoryDateFilterLastNDays(null, 30);
  }

  @Test
  public void resolvePreOrderDate_true_returnsDate() {
    Assertions.assertEquals(date, CommonUtils.resolvePreOrderDate(Boolean.TRUE, date));
  }

  @Test
  public void resolvePreOrderDate_false_returnsNull() {
    Assertions.assertNull(CommonUtils.resolvePreOrderDate(Boolean.FALSE, date));
  }

  @Test
  public void resolvePreOrderDate_null_returnsNull() {
    Assertions.assertNull(CommonUtils.resolvePreOrderDate(null, date));
  }
}
