package com.gdn.mta.product.util;

public final class BaseGenerator {

  public static Double generateShippingWeight(Double length, Double width, Double height, Double weight,
      Integer logisticAdjustment) throws Exception {
    Double volumeWeight = (length * width * height) / 6000;
    Double shippingWeight = 0.0;
    if (volumeWeight > weight) {
      shippingWeight = volumeWeight;
    } else {
      shippingWeight = weight;
    }
    shippingWeight *= logisticAdjustment / 100.0;
    return shippingWeight;
  }

}
