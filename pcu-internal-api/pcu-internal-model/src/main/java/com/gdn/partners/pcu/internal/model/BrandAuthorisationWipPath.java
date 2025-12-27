package com.gdn.partners.pcu.internal.model;

public interface BrandAuthorisationWipPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/brand-authorisation-wip";
  String DETAILS = "/details";
  String CREATE_BRAND_AUTH = "/create-brand-auth";
  String ACTION = "/action";
  String VALIDATE_BRAND_REQUEST = "/{brandCode}/{sellerCode}/validate-brand-auth-request";
  String UPDATE_BRAND_AUTH = "/update-authorisation-request";
  String FILTER_SUMMARY = "/filter-summary";
  String CREATION_ELIGIBILITY = "/creation-eligibility";
}
