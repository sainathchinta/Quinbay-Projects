package com.gdn.partners.pcu.external.service.impl.config;

import java.util.Date;

import org.apache.commons.beanutils.ConvertUtils;
import org.apache.commons.beanutils.converters.BooleanConverter;
import org.apache.commons.beanutils.converters.DateConverter;
import org.apache.commons.beanutils.converters.DoubleConverter;
import org.apache.commons.beanutils.converters.FloatConverter;
import org.apache.commons.beanutils.converters.IntegerConverter;
import org.apache.commons.beanutils.converters.LongConverter;
import org.apache.commons.beanutils.converters.ShortConverter;

public final class BeanUtilsConfigurer {
  private static IntegerConverter integerConverter = new IntegerConverter(null);
  private static IntegerConverter integerConverterPrimitive = new IntegerConverter(0);
  private static DoubleConverter doubleConverter = new DoubleConverter(null);
  private static DoubleConverter doubleConverterPrimitive = new DoubleConverter(0);
  private static LongConverter longConverter = new LongConverter(null);
  private static LongConverter longConverterPrimitive = new LongConverter(0);
  private static FloatConverter floatConverter = new FloatConverter(null);
  private static FloatConverter floatConverterPrimitive = new FloatConverter(0);
  private static BooleanConverter booleanConverter = new BooleanConverter(null);
  private static BooleanConverter booleanConverterPrimitive = new BooleanConverter(false);
  private static ShortConverter shortConverter = new ShortConverter(null);
  private static ShortConverter shortConverterPrimitive = new ShortConverter(0);
  private static DateConverter dateConverter = new DateConverter(null);

  private BeanUtilsConfigurer() {}

  public static void configure() {
    ConvertUtils.register(integerConverterPrimitive, Integer.TYPE);
    ConvertUtils.register(integerConverter, Integer.class);
    ConvertUtils.register(longConverterPrimitive, Long.TYPE);
    ConvertUtils.register(longConverter, Long.class);
    ConvertUtils.register(doubleConverterPrimitive, Double.TYPE);
    ConvertUtils.register(doubleConverter, Double.class);
    ConvertUtils.register(floatConverterPrimitive, Float.TYPE);
    ConvertUtils.register(floatConverter, Float.class);
    ConvertUtils.register(booleanConverterPrimitive, Boolean.TYPE);
    ConvertUtils.register(booleanConverter, Boolean.class);
    ConvertUtils.register(shortConverterPrimitive, Short.TYPE);
    ConvertUtils.register(shortConverter, Short.class);
    ConvertUtils.register(dateConverter, Date.class);
  }
}
