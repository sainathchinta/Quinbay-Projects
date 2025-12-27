package com.gdn.partners.pcu.master.model;

public enum ErrorCodes {

  INVALID_CATEGORY_ERROR("ERR-CAT-00001",
      "The selected category is deleted, inactive or is not a Cn category. Please select other to continue."),
  DIMENSION_ALREADY_EXISTS("ERR-DIM-00001", "Dimension name already exists"),
  SIZE_CHART_NAME_ALREADY_EXISTS("ERR-SIZ-00001","Size chart already exists"),
  ATTRIBUTE_IS_NOT_SIZE_ATTRIBUTE("ERR-SIZ-00002","attribute is not a size type attribute for mapping dimensions"),
  BRAND_NOT_FOUND("ERR-SIZ-00003","Brand not found : "),
  VALUE_TYPES_SELECTED_IS_NOT_MAPPED_TO_ATTRIBUTE("ERR-SIZ-00004","Value types selected are not mapped to attribute"),
  DIMENSION_MAPPING_DOES_NOT_EXIST_FOR_ATTRIBUTE_CODE("ERR-SIZ-00005","dimension mapping does not exist"),
  SIZE_CHART_NAME_CANNOT_BE_BLANK("ERR-SIZ-00006","Size chart name cannot be blank"),
  BUSINESS_PARTNER_CODE_CANNOT_BE_BLANK("ERR-SIZ-00007","Business partner code cannot be blank"),
  ATTRIBUTE_NAME_MUST_NOT_BE_BLANK("ERR-SIZ-00008","Attribute name must not be blank"),
  SELECTED_VALUE_TYPES_LIST_CANNOT_BE_EMPTY("ERR-SIZ-00009","Selected value types list cannot be empty"),
  SELECTED_DIMENSION_LIST_CANNOT_BE_EMPTY("ERR-SIZ-00010","Selected dimension list cannot be empty"),
  SIZE_CHART_GENDER_CANNOT_BE_BLANK("ERR-SIZ-00011","Size chart gender cannot be blank"),
  BRAND_NAME_MUST_NOT_BE_BLANK("ERR-SIZ-00012","Brand name shouldn't be blank"),
  GENDER_IS_INVALID("ERR-SIZ-00013","Gender is invalid"),
  SIZE_CHART_UNIT_CANNOT_BE_BLANK("ERR-SIZ-00014","Size chart unit cannot be blank"),
  SIZE_CHART_ROWS_CANNOT_BE_EMPTY("ERR-SIZ-00015","Size chart rows cannot be Empty"),
  SIZE_CHART_HEADER_MISMATCH("ERR-SIZ-00016","Size chart header mismatch"),
  INVALID_HEADER_TYPE_IN_SIZE_CHART("ERR-SIZ-00017","Invalid header type in size chart"),
  SELECTED_DIMENSION_AND_ROW_DIMENSION_DOES_NOT_MATCH("ERR-SIZ-00018","Selected dimension and row dimension does not match"),
  DIMENSION_VALUE_CANNOT_BE_EMPTY("ERR-SIZ-00019","Dimension value cannot be empty"),
  SELECTED_VALUE_TYPE_AND_ROW_VALUE_TYPE_DOES_NOT_MATCH("ERR-SIZ-00020","Selected value type and row value type does not match"),
  VALUE_TYPE_IS_NOT_SELECTED("ERR-SIZ-00021","Value type is not selected"),
  SIZE_CHART_NOT_FOUND_WITH_SIZE_CHART_CODE("ERR-SIZ-00022","Size chart not found with request size chart code"),
  SIZE_CHART_NAME_EXCEEDED_LENGTH("ERR-SIZ-00027", "Exceeds the size chart name length"),
  SIZE_CHART_INVALID_UNIT("ERR-SIZ-00026","Invalid size chart unit value"),
  DIMENSION_VALUE_CAN_EITHER_BE_SINGLE_OR_RANGE("ERR-SIZ-00028", "Dimension value can either be single or range"),
  DIMENSION_VALUE_CAN_ONLY_HAVE_NUMERIC_VALUES("ERR-SIZ-00024","Dimension value can only have numeric values"),
  MAX_VALUE_SHOULD_BE_GREATER_THAN_MIN_VALUE("ERR-SIZ-00025","Max value should be greater than min value"),
  SIZE_CHART_NAME_EDIT_NOT_ALLOWED_ERROR_CODE("ERR-SIZ-00023", "Size chart name edit not allowed"),
  SIZE_CHART_CANNOT_BE_DELETED("ERR-PBP100117", "Size chart cannot be deleted as there are products mapped to it");

  private final String errorCode;
  private final String errorMessage;

  ErrorCodes(String errorCode, String errorMessage) {
    this.errorCode = errorCode;
    this.errorMessage = errorMessage;
  }

  public String getErrorCode() {
    return errorCode;
  }

  public String getErrorMessage() {
    return errorMessage;
  }
}
