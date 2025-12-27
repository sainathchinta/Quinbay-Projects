package com.gdn.partners.pcu.internal.model;

public interface BrandAuthoriseApiPath {
  String BASE_PATH = Constants.CONTEXT_PATH + "/api/brandAuthorise";
  String GET_DETAIL_BY_BRAND_CODE = "/detail/{brandCode}";
  String DELETE = "/delete";
  String CREATE = "/create";
  String UPDATE = "/update";
  String HISTORY_SUMMARY = "/history/filter/summary";
  String LISTING = "/listing";
  String UPLOAD_BRAND_AUTH_DOC = "/uploadBrandAuthDoc";
  String BRAND_AUTH_BULK_UPLOAD = "/{processType}/brandAuthBulkUpload";
  String BRAND_AUTH_DOWNLOAD_ALL= "/all-bulk-download";
}
