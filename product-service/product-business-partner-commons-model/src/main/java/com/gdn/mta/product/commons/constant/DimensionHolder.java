package com.gdn.mta.product.commons.constant;

public interface DimensionHolder {
  Double getLength();

  void setLength(Double length);

  Double getWidth();

  void setWidth(Double width);

  Double getHeight();

  void setHeight(Double height);

  Double getWeight();

  void setWeight(Double weight);

  Double getShippingWeight();

  void setShippingWeight(Double sw);

  Boolean getB2cActivated();

  boolean isInstore();

  String getCategoryCode();
}
