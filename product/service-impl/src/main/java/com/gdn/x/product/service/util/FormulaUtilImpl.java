package com.gdn.x.product.service.util;

import java.util.Calendar;
import java.util.Set;
import java.util.TimeZone;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.service.api.SystemParameterService;

@Service
public class FormulaUtilImpl implements FormulaUtil {

  private static final Logger LOGGER = LoggerFactory.getLogger(FormulaUtilImpl.class);

  private static final String DASH = "-";

  @Autowired
  private SystemParameterService systemParameterService;

  private String appendWithHeadingZero(Number number, int totalDigit) {
    return String.format("%0" + totalDigit + "d", number);
  }

  @Override
  public String appendWithSerial(String base, Number number, int trailingLength) {
    StringBuilder sb = new StringBuilder();
    sb.append(base);
    sb.append(FormulaUtilImpl.DASH);
    sb.append(this.appendWithHeadingZero(number, trailingLength));
    return sb.toString();
  }

  private Calendar getCalendarInstance(int month, int year) {
    return this.getCalendarInstance(1, month, year);
  }

  private Calendar getCalendarInstance(int day, int month, int year) {
    Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
    calendar.set(Calendar.DAY_OF_MONTH, day);
    calendar.set(Calendar.MONTH, month);
    calendar.set(Calendar.YEAR, year);
    return calendar;
  }

  @Override
  public int getConcurrentSize(String storeId, Set<String> codes) {
    int size = codes.size();
    int concurrentSize = size;
    if (size > 1) {
      try {
        concurrentSize =
            Integer.valueOf(this.systemParameterService.findValueByStoreIdAndVariable(storeId,
                SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE).getValue());
      } catch (Exception e) {
        FormulaUtilImpl.LOGGER.error("failed to read masterDataConcurrentSize with error :",
            e.getMessage(), e);
        concurrentSize = 1;
      }
      if (concurrentSize > size) {
        return size;
      }
    }
    return concurrentSize;
  }

  @Override
  public DateTime getEndDate(int month, int year) {
    Calendar calendar = this.getCalendarInstance(month, year);
    calendar.set(Calendar.DAY_OF_MONTH, calendar.getActualMaximum(Calendar.DAY_OF_MONTH));
    this.setTimeToEndOfDay(calendar);
    return new DateTime(calendar);
  }

  @Override
  public DateTime getEndOfDay(int day, int month, int year) {
    Calendar calendar = this.getCalendarInstance(day, month, year);
    this.setTimeToEndOfDay(calendar);
    return new DateTime(calendar);
  }

  @Override
  public DateTime getStartDate(int month, int year) {
    Calendar calendar = this.getCalendarInstance(month, year);
    calendar.set(Calendar.DAY_OF_MONTH, calendar.getActualMinimum(Calendar.DAY_OF_MONTH));
    this.setTimeToBeginningOfDay(calendar);
    return new DateTime(calendar);
  }

  @Override
  public DateTime getStartOfDay(int day, int month, int year) {
    Calendar calendar = this.getCalendarInstance(day, month, year);
    this.setTimeToBeginningOfDay(calendar);
    return new DateTime(calendar);
  }

  private void setTimeToBeginningOfDay(Calendar calendar) {
    calendar.set(Calendar.HOUR_OF_DAY, 0);
    calendar.set(Calendar.MINUTE, 0);
    calendar.set(Calendar.SECOND, 0);
    calendar.set(Calendar.MILLISECOND, 0);
  }

  private void setTimeToEndOfDay(Calendar calendar) {
    calendar.set(Calendar.HOUR_OF_DAY, 23);
    calendar.set(Calendar.MINUTE, 59);
    calendar.set(Calendar.SECOND, 59);
    calendar.set(Calendar.MILLISECOND, 999);
  }
}
