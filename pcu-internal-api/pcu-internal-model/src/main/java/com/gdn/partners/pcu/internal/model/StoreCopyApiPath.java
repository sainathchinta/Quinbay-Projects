package com.gdn.partners.pcu.internal.model;

public interface StoreCopyApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/storeCopy";
  String DOWNLOAD_ALL_PRODUCTS = "/{sellerCode}/products/download";
  String DOWNLOAD_UPLOAD_TEMPLATE = "/{sellerCode}/template/download";
  String GET_PENDING_PROCESSES = "/get-pending-processes";
}
