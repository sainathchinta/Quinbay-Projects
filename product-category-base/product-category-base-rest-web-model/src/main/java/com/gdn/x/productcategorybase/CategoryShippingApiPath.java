package com.gdn.x.productcategorybase;

public interface CategoryShippingApiPath {

  String BASE_PATH = "/api/categoryShipping";

  String SAVE = "/save";
  String UPDATE = "/update";
  String DELETE_VALUE = "/delete_value";

  String FILTER_CATEGORY_CODE = "/filter/categorycode";
  String FILTER_SHIPPING_CODE = "/filter/shippingcode";
  String GENERATE_SHIPPING_WEIGHT = "/{categoryCode}/shippingWeight";
}
