package com.gdn.partners.pbp.commons.util;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pbp.commons.constants.Constants;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;

import java.beans.PropertyDescriptor;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Objects;
import java.util.Optional;
import java.util.TimeZone;

public final class CommonUtils {
  private static final String JKT_TIMEZONE= "Asia/Jakarta";
  private static final String DEFAULT_DATE_FORMAT = "yyyy-MM-dd HH:mm";
  private static final String ERROR_INCORRECT_DATE_REQUEST = "In-Correct date request.";

  private CommonUtils() {}

  public static String stringifyBean(Object bean) {
    if (bean != null) {
      BeanWrapper beanWrapper = new BeanWrapperImpl(bean);
      PropertyDescriptor[] propertyDescriptors = beanWrapper.getPropertyDescriptors();
      StringBuilder sb = new StringBuilder();
      sb.append(beanWrapper.getWrappedClass().getName());
      sb.append(" {");
      for (int i = 0; i < propertyDescriptors.length; i++) {
        PropertyDescriptor propDesc = propertyDescriptors[i];
        sb.append(propDesc.getName());
        sb.append(": ");
        sb.append(beanWrapper.getPropertyValue(propDesc.getName()));
        if (i < propertyDescriptors.length - 1) {
          sb.append(", ");
        }
      }
      sb.append(" }");
      return sb.toString();
    } else {
      return null;
    }
  }

  public static String toJakartaTimeZone(Date date) throws UnsupportedEncodingException, ParseException {
    TimeZone timeZone = TimeZone.getTimeZone(JKT_TIMEZONE);
    SimpleDateFormat dateFormat = new SimpleDateFormat(DEFAULT_DATE_FORMAT);
    dateFormat.setTimeZone(timeZone);
    ZonedDateTime nowAsiaJakarta = ZonedDateTime
        .ofInstant(dateFormat.parse(URLDecoder.decode(dateFormat.format(date), "UTF-8")).toInstant(),
            ZoneId.of("Asia/Jakarta"));
    return nowAsiaJakarta.format(DateTimeFormatter.ISO_INSTANT);
  }

  public static Date atJakartaStartOfDay(Date date) {
    if (Objects.isNull(date)) {
      return null;
    }
    LocalDate jakartaLocalDate = date.toInstant().atZone(ZoneId.of(JKT_TIMEZONE)).toLocalDate();
    return Date.from(jakartaLocalDate.atStartOfDay(ZoneId.of(JKT_TIMEZONE)).toInstant());
  }

  public static Date resolvePreOrderDate(Boolean isPreOrder, Date preOrderDate) {
    return Boolean.TRUE.equals(isPreOrder) ? preOrderDate : null;
  }

  public static void validateHistoryDateFilterLastNMonths(Date date, int months) {
    LocalDate localCurrentDate = getCurrentLocalDate();
    if (Objects.nonNull(date)) {
      LocalDate localStartDate = toJakartaLocalDate(date);
      if (localStartDate.isAfter(localCurrentDate) || localStartDate.isBefore(localCurrentDate.minusMonths(months))) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ERROR_INCORRECT_DATE_REQUEST);
      }
    }
  }

  public static void validateHistoryDateFilterLastNDays(Date date, int days) {
    LocalDate localCurrentDate = getCurrentLocalDate();
    if (Objects.nonNull(date)) {
      LocalDate localStartDate = toJakartaLocalDate(date);
      if (localStartDate.isAfter(localCurrentDate) || localStartDate.isBefore(localCurrentDate.minusDays(days))) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ERROR_INCORRECT_DATE_REQUEST);
      }
    }
  }

  public static void validateHistoryDateFilterAfterLastNMonths(Date date, int months) {
    LocalDate localCurrentDate = getCurrentLocalDate();
    if (Objects.nonNull(date)) {
      LocalDate localStartDate = toJakartaLocalDate(date);
      if (localStartDate.isAfter(localCurrentDate) || localStartDate.isAfter(localCurrentDate.minusMonths(months))) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ERROR_INCORRECT_DATE_REQUEST);
      }
    }
  }

  public static void validateStartDateIsBeforeEndDate(Date startDate, Date endDate) {
    if (Objects.nonNull(startDate) && Objects.nonNull(endDate)) {
      LocalDate localStartDate = toJakartaLocalDate(startDate);
      LocalDate localEndDate = toJakartaLocalDate(endDate);
      if (localStartDate.isAfter(localEndDate)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ERROR_INCORRECT_DATE_REQUEST);
      }
    }
  }

  private static LocalDate toJakartaLocalDate(Date date) {
    return date.toInstant().atZone(ZoneId.of(JKT_TIMEZONE)).toLocalDate();
  }

  private static LocalDate getCurrentLocalDate() {
    return LocalDate.now(ZoneId.of(JKT_TIMEZONE));
  }

  public static String getItemSkuAndPickupPointKey(String itemSku, String pickupPointCode) {
    return new StringBuilder(Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY)).append(Constants.HYPHEN)
        .append(Optional.ofNullable(pickupPointCode).orElse(StringUtils.EMPTY)).toString();
  }
}
