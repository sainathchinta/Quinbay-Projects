package com.gdn.partners.pcu.external.model;

public interface ProductAssemblyApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/product-assembly";
  String GET_WAREHOUSE_CODE_AND_NAME_LIST = "/getWarehouseCodeAndFulfillmentCenter";
  String GET_REQUEST_FORMS_LISTING = "/listing";
  String CREATE_REQUEST = "/{type}/create";
  String GET_HISTORY = "/get-request-history";
  String RETRY_OR_CANCEL = "/cancel-or-retry-request";

}
