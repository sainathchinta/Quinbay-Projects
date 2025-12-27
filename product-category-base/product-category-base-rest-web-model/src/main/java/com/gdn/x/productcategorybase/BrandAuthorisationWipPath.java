package com.gdn.x.productcategorybase;

public interface BrandAuthorisationWipPath {
  String BASE_PATH = "/api/brand-authorisation-wip";
  String APPROVE = "/approve";
  String SUBMIT_AUTHORISATION_REQUEST = "/submit-authorisation-request";
  String DETAILS = "/details";
  String CREATE_BRAND_AUTH = "/create-brand-auth";
  String VALIDATE_BRAND_REQUEST = "/{brandCode}/{sellerCode}/validate-brand-auth-request";
  String FILTER_SUMMARY = "/filter/summary";
  String CREATION_ELIGIBILITY = "/creation-eligibility";
}
