package com.gdn.x.productcategorybase.util;

import java.beans.FeatureDescriptor;
import java.util.Objects;
import java.util.stream.Stream;

import org.apache.commons.lang3.ArrayUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;

public class ObjectCopyUtil {

  public static void copyPropertiesIgnoreNullValues(Object src, Object target, String... ignoreProperties) {
    String[] nullValueProperties = getNullPropertyNames(src);
    String[] fullIgnoreProperties = ArrayUtils.addAll(nullValueProperties, ignoreProperties);
    BeanUtils.copyProperties(src, target, fullIgnoreProperties);
  }

  private static String[] getNullPropertyNames(Object source) {
    final BeanWrapper wrappedSource = new BeanWrapperImpl(source);
    return Stream.of(wrappedSource.getPropertyDescriptors())
        .map(FeatureDescriptor::getName)
        .filter(propertyName -> Objects.isNull(wrappedSource.getPropertyValue(propertyName)))
        .toArray(String[]::new);
  }
}
