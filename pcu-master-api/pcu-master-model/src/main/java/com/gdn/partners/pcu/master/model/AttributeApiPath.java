package com.gdn.partners.pcu.master.model;

public interface AttributeApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/attributes";
  String GET_ATTRIBUTE_INFO = "/info/{attributeCode:.+}";
  String GET_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODE = "/{attributeCode}/values";
  String UPDATE_ATTRIBUTE_DETAIL = "/update/{attributeCode}";
  String FILTER = "/filter";
  String UPDATE_ATTRIBUTE_VALUES = "/{attributeCode}/values";
  String ATTRIBUTES_LIST_BY_ATTRIBUTE_TYPE = "/attributeType/{attributeType}";
  String GET_ALL_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODE = "/{attributeCode}/allValues";
  String GET_ALL_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODES = "/getAttributeValuesByAttributeCodes";
  String GET_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_BY_ATTRIBUTE_ID_AND_VALUE =
      "/predefined/attributeId/{attributeId}/value/{value}";
  String ADD_ATTRIBUTE_VALUE_BY_ATTRIBUTE_CODE = "/{attributeCode}/values";
  String GET_SPECIFIC_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_BY_ATTRIBUTE_ID_AND_VALUE = "/predefined/attributeId/value";
  String GET_DETAIL_BY_ATTRIBUTE_CODE = "/detail/{attributeCode}";
}
