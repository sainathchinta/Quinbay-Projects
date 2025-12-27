package com.gdn.x.product.service.util;

import java.util.Set;

import org.joda.time.DateTime;


public interface FormulaUtil {

  /**
   * Append the base object code with trailing serial number. Example = base is BASE, number is 3,
   * trailingLength is 5, then will return BASE-00003
   *
   * @param base the base object code
   * @param number the trailing serial number
   * @param trailingLength the trailing serial length
   * @return String appended
   */
  String appendWithSerial(String base, Number number, int trailingLength);

  int getConcurrentSize(String storeId, Set<String> codes);

  /**
   * Get the very end of the date of specified month of the year. Example = month is 1 (feb), year
   * is 2016, then will return 29 February 2016, 23:59:59.999
   *
   * @param month base java month 0 based
   * @param year the year
   * @return Datetime of the very end of month
   */
  DateTime getEndDate(int month, int year);

  /**
   * Get the very end of the date of specified day and month of the year. Example = day is 1, month
   * is 1 (feb), year is 2016, then will return 1 February 2016, 23:59:59.999
   *
   * @param day weekday type day
   * @param month base java month 0 based
   * @param year the year
   * @return Datetime of the very end of month
   */
  DateTime getEndOfDay(int day, int month, int year);

  /**
   * Get the very start of the date of specified month of the year. Example = month is 1 (feb), year
   * is 2016, then will return 1 February 2016, 00:00:00.000
   *
   * @param month base java month 0 based
   * @param year the year
   * @return Datetime of the very end of month
   */
  DateTime getStartDate(int month, int year);

  /**
   * Get the very start of the date of specified day and month of the year. Example = day is 1,
   * month is 1 (feb), year is 2016, then will return 1 February 2016, 00:00:00.000
   *
   * @param day weekday type day
   * @param month base java month 0 based
   * @param year the year
   * @return Datetime of the very end of month
   */
  DateTime getStartOfDay(int day, int month, int year);

}
