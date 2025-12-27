package com.gdn.x.productcategorybase.dto;

public enum SizeChartHeaderType {
  DIMENSION, VALUE_TYPE;

  public static boolean validSizeChartHeaderType(String name) {
    for (SizeChartHeaderType sizeChartHeaderType : SizeChartHeaderType.values()) {
      if (sizeChartHeaderType.name().equals(name)) {
        return true;
      }
    }
    return false;
  }
}
