package com.gdn.mta.product.web.model;

public interface ProductSystemParameterControllerPath {
  String BASE_PATH = "/api/productSystemParameter";
  String SYSTEM_PARAMETER_INSERT = "";
  String SYSTEM_PARAMETER_UPDATE = "/update";
  String SYSTEM_PARAMETER_FIND = "/find";
  String SYSTEM_PARAMETER_DELETE = "/{variable}";
  String SYSTEM_PARAMETER_SWITCH_FETCH = "/fetchSwitchValues";
  String SYSTEM_PARAMETER_FETCH_WITH_SHOW_ON_UI = "/fetchSwitchValuesWithShowOnUI";
}
