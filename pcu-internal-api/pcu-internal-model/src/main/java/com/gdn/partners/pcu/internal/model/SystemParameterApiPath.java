package com.gdn.partners.pcu.internal.model;

public interface SystemParameterApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/system-parameter";
  String UPDATE_SYSTEM_PARAMETER = "/update";
  String GET_ALL_SYSTEM_PARAMETER_WITH_SHOW_ON_UI = "/get-ui-system-parameter";
  String FETCH_SYSTEM_PARAMETER = "/fetch-system-parameters";
}
