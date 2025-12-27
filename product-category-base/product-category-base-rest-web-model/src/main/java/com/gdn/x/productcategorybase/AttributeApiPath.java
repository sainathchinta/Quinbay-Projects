package com.gdn.x.productcategorybase;

public interface AttributeApiPath {

  String BASE_PATH = "/api/attribute";

  String DETAIL = "/{id}";
  String FIND_ATTRIBUTE_DETAIL_BY_ID_AND_VALUE = "/{id}/values/{value:.+}";
  String SAVE = "/save";
  String UPDATE = "/update";
  String UPDATE_MASTER_ATTRIBUTE = "/update-master-attribute";
  String UPLOAD = "/upload";
  String DELETE_VALUE = "/delete_value";
  String INFO = "/info/{attributeCode:.+}";
  String DETAIL_BY_ATTRIBUTE_CODE = "/detail";
  String UPDATE_VALUES = "/{attributeCode}/values";
  String ADD_VALUE = "/{attributeCode}/values";

  String FILTER_ATTRIBUTE_CODE = "/filter/attributecode";
  String FILTER_ATTRIBUTE_CODES = "/filter/attributecodes";
  String FILTER_NAME = "/filter/name";
  String FILTER_NAME_LIKE = "/filter/nameLike";
  String FILTER_TYPE = "/filter/type";
  String FILTER_SEARCHABLE_FALSE = "/filter/searchablefalse";
  String FILTER_SEARCHABLE_TRUE = "/filter/searchabletrue";
  String FILTER_ATTRIBUTE_DETAIL_BY_CATEGORY_CODE = "/filter/attributeDetailByCategoryCode";
  String FILTER_ATTRIBUTE_DETAIL_BY_CATEGORY_CODE_WITHOUT_OPTIONS = "/filter/attributeDetailByCategoryCode/withoutOptions";
  String FILTER_ATTRIBUTE_VALUES_BY_ATTRIBUTE_CODE = "/{attributeCode}/values";
  String MASTER_ATTRIBUTE_BASE_PATH = "/api/master-attribute";
  String FILTER_LIST = "/filter/list";
  String GET_DETAIL_BY_ATTRIBUTE_CODE = "attributeCode/{attributeCode}/detail";
  String GET_ATTRIBUTE_VALUES = "/getAttributeValuesByProductCodeAndAttributeCode";
  String CATEGORIES_BY_ATTRIBUTE_CODE = "/categoriesByAttributeCode";
}
