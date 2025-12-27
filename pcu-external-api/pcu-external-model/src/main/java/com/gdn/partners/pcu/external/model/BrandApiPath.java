package com.gdn.partners.pcu.external.model;

public interface BrandApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/brands";
  String GET_BRAND_SUGGESTIONS = "/suggestions";
  String FILTER_SUMMARY = "/filter/summary";
  String FILTER_BRAND_NAME = "/filter/brand-name";
  String VALIDATE = "/valid";
}
