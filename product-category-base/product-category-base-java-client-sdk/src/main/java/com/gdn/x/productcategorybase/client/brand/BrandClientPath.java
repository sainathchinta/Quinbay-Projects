package com.gdn.x.productcategorybase.client.brand;

public interface BrandClientPath {

  String BASE_PATH = "/api/brand";
  String CREATE = "/create";
  String UPDATE = "/update";
  String DELETE = "/delete";
  String UNDELETE = "/undelete";
  String FILTER_BRAND_CODE = "/filter/brand-code";
  String GET_BRAND_NAMES_BY_CODES = "/filter/brand-codes";
  String FILTER_BRAND_NAME = "/filter/brand-name";
  String FILTER_SUMMARY = "/filter/summary";
  String FILTER_SUMMARY_ORDER_BY_NAME = "/filter/summary/name";

}
