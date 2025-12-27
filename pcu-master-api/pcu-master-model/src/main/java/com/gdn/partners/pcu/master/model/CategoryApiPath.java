package com.gdn.partners.pcu.master.model;

public interface CategoryApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/categories";
  String ACTIVATE = "/{categoryCode}/activate";
  String DEACTIVATE = "/{categoryCode}/deactivate";
  String UPDATE_CATEGORY_INFO = "/{categoryCode}";
  String UPDATE_CATEGORY_MAPPINGS = "/{categoryId}/mappings";
  String INFO = "/{categoryId}";
  String CREATE = "";
  String GET_CATEGORY_MAPPINGS_BY_CATEGORY_CODES = "/getCategoryMappingsByCategoryCodes";
  String GET_MARGIN_BY_CATEGORY_CODE = "/{categoryCode}/margin";
  String GET_USED_SEQUENCE = "/get-used-sequence/{parentId}";
  String UPDATE_CATEGORY_RESTRICTED_KEYWORD_MAPPINGS = "/{categoryCode}/restricted-keywords";
  String GET_MARGIN_BY_BUSINESS_PARTNER_CODE_AND_CATEGORY_CODE = "/{businessPartnerCode}" + GET_MARGIN_BY_CATEGORY_CODE;
  String GET_CATEGORY_RESTRICTED_KEYWORD = "/get-restricted-keywords";
  String GET_CATEGORY_WHOLESALE_CONFIG = "/get-wholesale-config/mappings";
  String UPDATE_CATEGORY_WHOLESALE_CONFIG_MAPPING = "/{categoryId}/wholesale-config";
  String GET_CATEGORY_LIST_FOR_GENERIC_TEMPLATE = "/getCategoryListForGenericTemplate";
  String GET_CATEGORY_TREE = "/getCategoryTree";
  String GET_DOCUMENT_LIST = "/getSupportingDocuments";

  String GET_SYSTEM_PARAM_VALUE = "/getSystemParamValue";
  String GET_CATEGORY_SUGGESTIONS = "/suggestions/product-name";
  String VALIDATE_CATEGORY = "/{categoryId}/validate-category";
  String ADD_ORIGINAL_SALES_CATEGORY = "/addOriginalSalesCategory";
  String FETCH_OSC_LIST = "/fetch-osc-list";
  String UPDATE_OSC_LIST = "/update-osc-list";
  String GET_ORIGINAL_SALES_CATEGORY_DETAILS_BY_ID = "/originalSalesCategory/{id}";
  String GET_VALUE_FROM_PROPERTIES = "/getValueFromProperties";
  String FETCH_BASE_MARGIN_HIERARCHY = "/{categoryCode}/baseMarginHierarchy";
  String PROFIT_MARGIN = "/profitMargin";
}
