package com.gdn.partners.pcu.external.model;

public interface ProductBusinessPartnerApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/productBusinessPartners";
  String CREATE = "/create";
  String GET_BUSINESS_PARTNER_PROFILE_BY_ID = "/getBusinessPartnerProfileById";
  String COPY_PRODUCT_ITEMS = "/copy";
  String COPY_ALL_PRODUCT_ITEMS = "/copy/all";
  String RETRY_COPY_PRODUCT_ITEMS = "/retryCopy";
  String PRODUCT_ITEMS_TO_COPY = "/filter/products/copy";
  String IS_PRODUCT_MAPPED_TO_MERCHANT = "/isProductMapped";
  String FETCH_PICKUP_POINT_DETAIL_SUMMARY = "/fetch-pickup-points-details";
  String SAVE_DEFAULT_PICKUP_POINT_AND_CONFIG = "/{merchantCode}/save-default-pickuppoints-and-configuration";
}
