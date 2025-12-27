package com.gdn.x.productcategorybase;

public interface PredefinedAllowedAttributeValueApiPath {
  String BASE_PATH = "/api/predefinedAllowedAttributeValue";

  String FILTER_ATTRIBUTE_ID_AND_VALUE = "/filter/getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable";
  String GET_ATTRIBUTE_BY_ID_AND_VALUE = "/filter/getSpecificPredefinedAllowedAttributeValueByAttributeIdAndValue";
  String GET_BY_ATTRIBUTE_CODE_AND_VALUE = "/filter/getPredefinedAllowedAttributeValueByAttributeCodeAndValue";
  String FILTER_ATTRIBUTE_ID_AND_VALUE_FOR_SUGGESTIONS = "/filter/getBrandSuggestions";
  String DEACTIVATED = "/deactivated";
  String SAVE = "/save";
}
