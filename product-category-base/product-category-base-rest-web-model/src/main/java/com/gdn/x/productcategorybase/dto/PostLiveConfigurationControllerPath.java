package com.gdn.x.productcategorybase.dto;

public interface PostLiveConfigurationControllerPath {

  String BASE_PATH = "/api/postlive-config";
  String FETCH_MERCHANT_CONFIGURATION = "/fetchMerchantConfig";
  String ADD_CATEGORY_CONFIGURATION = "/categories";
  String UPDATE_CATEGORY_CONFIGURATION = "/categories";
  String DELETE_CATEGORY_CONFIGURATION = "/categories/{categoryCode}";
  String ADD_MERCHANT_CONFIGURATION = "/merchants";
  String UPDATE_MERCHANT_CONFIGURATION = "/merchants";
  String DELETE_MERCHANT_CONFIGURATION = "/merchants/{merchantCode}";
  String GET_CONFIGURATION_STATUS = "/get-configuration-status";
  String FETCH_CONFIGURATION_COUNT = "/fetchConfigurationCounts";
  String BULK_CONFIG_MERCHANT = "/merchants/bulk-add";
  String BULK_CONFIG_CATEGORY = "/categories/bulk-add";
  String GET_CONFIGURATION_CHANGES = "/configuration-changes";
  String FILTER_CATEGORY_CONFIGURATION = "/getCategoryConfigurationList";
  String FILTER_MERCHANT_CONFIGURATION = "/getMerchantConfigurationList";
  String CATEGORY_CONFIGURATION_HISTORY = "/getCategoryConfigurationHistory/{categoryCode}";
  String MERCHANT_CONFIGURATION_HISTORY = "/getMerchantConfigurationHistory/{merchantCode}";
  String FETCH_DETAILS_BY_CODE = "/getConfigDetailsByCodes";
}
