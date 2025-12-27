package com.gdn.partners.pcu.external.model;

public interface GeneratorApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/generators";
  String GENERATE_PRODUCT_CODE = "/generateProductCode";
  String GENERATE_BAR_CODE = "/generateBarCode";
  String GENERATE_SHIPPING_WEIGHT = "/{categoryCode}/shippingWeight";
}
