package com.gdn.mta.product.service.generator;

public interface GeneratorService {

  Double generateShippingWeight(Double length, Double width, Double height, Double weight, String categoryCode)
      throws Exception;

}
