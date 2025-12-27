package com.gdn.partners.pcu.internal.model;

public interface PostLiveConfigurationControllerPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/postlive-config";
  String MERCHANT_SEARCH = "/merchants/{searchKeyword}";
  String ADD_CATEGORY_CONFIGURATION = "/categories";
  String UPDATE_CATEGORY_CONFIGURATION = "/categories/{categoryCode}";
  String DELETE_CATEGORY_CONFIGURATION = "/categories/{categoryCode}";
  String ADD_MERCHANT_CONFIGURATION = "/merchants";
  String UPDATE_MERCHANT_CONFIGURATION = "/merchants/{merchantCode}";
  String DELETE_MERCHANT_CONFIGURATION = "/merchants/{merchantCode}";
  String BULK_CONFIGURATION_UPLOAD = "/bulk/update";
  String BULK_CONFIGURATION_DOWNLOAD = "/bulk/download";
  String CONFIGURATIONS_STATUS = "/get-configuration";
  String CONFIGURATION_COUNT = "/configuration-counts";
  String CATEGORY_LISTING = "/category-filter-summary";
  String MERCHANT_LISTING = "/merchant-filter-summary";
  String CATEGORY_CONFIGURATION_HISTORY = "/category/{category-code}/history";
  String MERCHANT_HISTORY = "/merchant/{merchant-code}/history";
}
