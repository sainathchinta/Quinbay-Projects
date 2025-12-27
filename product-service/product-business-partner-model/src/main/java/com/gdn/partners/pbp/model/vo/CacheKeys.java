package com.gdn.partners.pbp.model.vo;

public interface CacheKeys {
  String ROOT_KEY = "com.gdn.mta.pbp.";
  String SYSPARAM_GROUP_KEY = ROOT_KEY + "sysparam.";
  String INDEXING_STATE = ROOT_KEY + "indexing.state";
  String BRANDS_BY_CATEGORY_CODE = ROOT_KEY + "getAllBrandsByCategoryCode";
  String CATALOG_CODE = ROOT_KEY + "catalogCode";
  String PRISTINE_CATEGORY = ROOT_KEY + "supportedCategoriesByPristine";
  // public static final String GLOBAL_SYSPARAM_GROUP_KEY = ROOT_KEY + "global.sysparam.";
  String PRODUCT_SYSTEM_PARAMETER = ROOT_KEY + "productSystemParameter";
  String PRODUCT_SYSTEM_PARAMETER_SWITCHES = ROOT_KEY + "productSystemParameterSwitches";
  String PRODUCT_IMAGE_PREDICTION = ROOT_KEY + "productImagePrediction";
  String PRODUCT_IMAGE_PREDICTION_TYPE = ROOT_KEY + "productImagePredictionType";
  String PRODUCT_IMAGE_PREDICTION_RESPONSE = ROOT_KEY + "productImagePredictionResponse";
  String AUTO_APPROVAL_RULES = ROOT_KEY + "autoApprovalRules";
  String PRODUCT_LIMITS = ROOT_KEY + "productLimit";
  String COUNT_PRODUCT_LEVEL3_WIP_GROUP_BY_ACTIVE_AND_STATE = ROOT_KEY +
    "countProductLevel3WipByStateAndActiveFlag";
  String COUNT_PRODUCT_LEVEL3_WIP_FOR_INACTIVE_PRODUCTS_GROUP_BY_STATE = ROOT_KEY +
    "countProductLevel3WipForInActiveProductsByState";


}
