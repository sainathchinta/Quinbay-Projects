package com.gdn.x.productcategorybase;

public interface BrandControllerPath {

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
  String FILTER_BRAND_BY_CODE_AND_STATUS = "/filter/{brandCode}/status/{status}";
  String GET_DEFAULT_BRANDS = "/getDefaultBrands";
  String GET_VALID_BRAND_SUMMARY = "/validBrandSummary";
  String PROTECTED_BRANDS = "/protectedBrands";
}
