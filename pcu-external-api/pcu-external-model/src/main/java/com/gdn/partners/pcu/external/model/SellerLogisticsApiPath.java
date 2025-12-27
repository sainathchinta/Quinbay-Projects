package com.gdn.partners.pcu.external.model;

public interface SellerLogisticsApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/sellerLogistics";
  String GET_SELER_LOGISTICS_PRODUCT = "/getSellerLogistics";
  String GET_TEMPLATE_DATA = "/downloadTemplate";
  String UPLOAD_LOGISTICS_TEMPLATE_UPDATE = "/uploadExcel";
  String GET_UPLOAD_STATUS = "/uploadExcelStatus";

}
