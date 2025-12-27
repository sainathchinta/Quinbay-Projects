package com.gdn.partners.pcu.internal.model;

public interface QCApiPath {
  String BASE_PATH = Constants.CONTEXT_PATH + "/api/finalQC";
  String FILTER_QC_READY_PRODUCT = "/productList";
  String RETRY_PRODUCT_ACTIVATION = "/retryProductActivation";
  String REJECT_PRODUCT = "/rejectProduct";
  String APPROVE_PRODUCT = "/approveProduct";
  String BUSINESS_PARTNER_LIST = "/businessPartnerList";
}
