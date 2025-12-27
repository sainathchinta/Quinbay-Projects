package com.gdn.partners.pcu.internal.model;

public interface AutoApprovedApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/product-analytics";

  String AUTO_APPROVED_PRODUCTS_PATH = "/auto-approved-products-list";
  String PERFORM_ACTION_ON_AUTO_APPROVED_PRODUCTS = "/{productCode}/auto-approved-product-action";
  String AUTO_APPROVED_PRODUCT_DETAILS_API_PATH = "/{productCode}/detail";
  String UPDATE_ASSIGNEE = "/update-assignee";
  String UPDATE_USER_FEEDBACK = "/{productCode}/update-user-feedback";
  String FETCH_USER_FEEDBACK = "/{productCode}/fetch-user-feedback";
  String AUTO_APPROVED_PRODUCT_DOWNLOAD = "/auto-approved-product-download";
  String AUTO_APPROVED_PRODUCT_UPLOAD_ASSIGNEE = "/auto-approved-product-upload-assignee";
}
