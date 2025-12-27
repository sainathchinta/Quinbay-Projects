package com.gdn.partners.pcu.external.service;

public interface GeneratorService {

  /**
   * To generate productCode
   *
   * @return productCode
   */
  String generateProductCode();

  /**
   * To generate barCode
   *
   * @return barCode
   */
  String generateBarCode();

  /**
   * To generate shipping weight
   *
   * @param categoryCode
   * @param length
   * @param width
   * @param height
   * @param weight
   * @return Shipping weight
   */
  Double generateShippingWeight(String categoryCode, double length, double width, double height, double weight);
}
