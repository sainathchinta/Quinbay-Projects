package com.gdn.x.productcategorybase;

public interface SystemParameterControllerPath {
  String BASE_PATH = "/api/systemParameter";
  String SYSTEM_PARAMETER_INSERT = "";
  String SYSTEM_PARAMETER_UPDATE = "/update";
  String SYSTEM_PARAMETER_FIND = "/find";
  String SYSTEM_PARAMETER_DELETE = "/{variable}";
}
