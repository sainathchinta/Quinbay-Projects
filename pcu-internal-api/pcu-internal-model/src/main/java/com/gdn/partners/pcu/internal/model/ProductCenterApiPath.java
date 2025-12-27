package com.gdn.partners.pcu.internal.model;

public interface ProductCenterApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/product-center";
  String PRODUCT_CENTER_SUMMARY_FILTER = "/filter/keyword";
  String PRODUCT_CENTER_LISTING_ACTION = "/updateSalesCategory";
  String PRODUCT_CENTER_DETAIL = "/{productSku}/detail";
  String PRODUCT_CENTER_DETAIL_UPDATE = "/{productSku}/update";
  String PRODUCT_CENTER_HISTORY = "/{productSku}/sales-category-history";
  String PRODUCT_CENTER_DOWNLOAD_UNMAPPED_SKUS = "/{category-code}/unmapped-skus";
}
