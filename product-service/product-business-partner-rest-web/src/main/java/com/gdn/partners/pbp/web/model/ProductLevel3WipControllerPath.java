package com.gdn.partners.pbp.web.model;

public interface ProductLevel3WipControllerPath {

  String BASE_PATH = "/api/product-level3/wip";
  String DETAIL_FILTER_PRODUCT_SKU = "/detail/filter/product-sku";
  String FILTER_SUMMARY_WITH_STATE = "/filter/summary-with-state";
  String COUNT_SUMMARY_WITH_STATE = "/count/summary-with-state";
  String COUNT_SUMMARY_WITH_FILTER_STATE = "/count/summary-by-filter-type";
  String FILTER_SUMMARY_WITH_EXPECTATION_ACTIVATION_DATE = "/filter/summary-with-expectation-activation-date";
  String SEND_MAIL_FOR_EXCEEDED_ACTIVATION = "/send-mail-for-exceeded-activation";

}
