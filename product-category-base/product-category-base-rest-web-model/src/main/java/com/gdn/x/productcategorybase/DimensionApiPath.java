package com.gdn.x.productcategorybase;

public interface DimensionApiPath {
  String BASE_PATH = "/api/dimensions";
  String DETAIL = "/{dimensionCode}/detail";
  String FILTER = "/filter";
  String SAVE = "/save";
  String DIMENSION_MAPPING_LIST = "/{attributeCode}/dimension-mapping";
  String EDIT = "/edit";
  String FIND_BY_NAME = "/findByName";
  String UPDATE_DIMENSION_MAPPING = "/{attributeCode}/update-dimension-mapping";
}
