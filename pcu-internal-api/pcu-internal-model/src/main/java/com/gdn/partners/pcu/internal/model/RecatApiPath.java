package com.gdn.partners.pcu.internal.model;

public interface RecatApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/recat";
  String UPLOAD_EXCEL = "/upload";
  String GET_FAILED_PRODUCTS_MAIL = "/{recat-request-code}/get-failed-products-mail";
  String REQUEST_SUMMARY_FILTER = "/request-summary";
  String CANCEL_REQUEST = "/{recatRequestCode}/cancel-request";
  String GET_RECAT_PRODUCT_SUMMARY = "/{recat-request-code}/product-summary";
  String GET_REACT_PRODUCT_STATUS_COUNTS = "/{recat-request-code}/product-status-counts";
}
