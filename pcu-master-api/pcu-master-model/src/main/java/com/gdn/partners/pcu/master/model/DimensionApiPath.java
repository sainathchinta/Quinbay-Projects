package com.gdn.partners.pcu.master.model;

public interface DimensionApiPath {

    String BASE_PATH = Constants.CONTEXT_PATH + "/api/dimensions";
    String FILTER = "/filter";
    String FETCH_DIMENSION_DETAIL_BY_DIMENSION_CODE = "/{dimensionCode}/detail";
    String CREATE = "/create";
    String DIMENSION_MAPPING_LIST = "/{attributeCode}/dimension-mapping";
    String UPDATE_DIMENSION_MAPPING_LIST = "/{attributeCode}/update-dimension-mapping";
    String EDIT = "/{dimensionCode}/edit";
    String VALIDATE = "/validate";
}
