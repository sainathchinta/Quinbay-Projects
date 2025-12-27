package com.gdn.x.productcategorybase;

public interface BrandAuthorisationPath {

  String BASE_PATH = "/api/brandAuthorise";
  String VALID = "/{sellerCode}/valid";
  String GET_AUTHORISATIONS = "/getAuthorisations";
  String TAKE_DOWN_BASED_ON_BRAND = "/isTakeDown";
  String BRAND_AUTHORISE_DETAIL_BY_BRAND_CODE = "/detail/{brandCode}";
  String CREATE = "/create";
  String DELETE = "/delete";
  String HISTORY_FILTER_SUMMARY = "/history/filter/summary";
  String GET_BRAND_AUTH_BY_IDS = "/details/brandAuth";
  String UPDATE = "/update";
}
