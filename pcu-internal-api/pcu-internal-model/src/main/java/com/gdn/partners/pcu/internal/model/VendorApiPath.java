package com.gdn.partners.pcu.internal.model;

public interface VendorApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/vendor";
  String BUSINESS_PARTNERS_LIST = "/getBusinessPartnerList";
  String PRODUCT_HISTORY_FOR_VENDOR = "/{productCode}/history";
  String PRODUCT_CORRECTION_ACTION = "/product-need-correction";
  String GET_PRODUCT_DETAILS = "/detail/product-code/{productCode}";
  String ASSIGNEE_LIST = "/getAssigneeList";
  String VENDOR_PRODUCT_UPDATE = "/update/{type}";
  String VENDOR_PRODUCT_APPROVE = "/approve";
  String VENDOR_PRODUCT_LIST = "/filter/summary";
  String VENDOR_PRODUCT_ASSIGN_UNASSIGN = "/products/{action}/action";
  String PRIMARY_FILTER = "/filterCounts";
  String PRODUCT_REVIEW_CONFIG_COUNTS = "/productReviewConfigCounts";
  String DOWNLOAD_FILTERED_PRODUCT_VENDOR = "/download-filtered-products";
  String VENDOR_BULK_REJECT = "/products/reject";
  String PRODUCT_SCREENING_NOTES = "/screenerNotes/{productCode}";
  String SEND_PRODUCT_BACK_TO_VENDOR = "/sendProductBackToVendor/{productCode}";
  String BULK_PRODUCT_ASSIGN = "/bulkUpdateProductAssignment";
  String VENDORS_LIST = "/getVendorsList";
  String GET_IMAGE_FEEDBACK = "/image-feedback/product-code/{productCode}";
  String UPDATE_IMAGE_FEEDBACK = "/update/image-feedback";
  String GET_IMAGE_FAULTY_TYPE = "/image-faulty-type";
  String DETECT_EDIT_BY_MERCHANT = "/{productCode}/detect-edit-by-merchant";
  String REINDEX_PRODUCT = "/{productCode}/reindex-product";
  String PENDING_UPLOAD = "/pending-upload";
  String GET_REVIEW_CONFIG_COUNT = "/getReviewConfigCount";
  String REPUBLISH_EDITED_PRODUCT = "/republishEditedProduct/{productCode}";
  String VENDOR_DETAIL = "/user/detail/{vendorCode}";
  String QUICK_PRODUCT_APPROVAL = "/product/quick-approve";
  String PRODUCT_REVIEWERS = "/product/reviewers";
  String AUTO_ASSIGN_PRODUCTS = "/autoAssignProducts";
  String GET_DEFAULT_SETTING = "/getDefaultSetting";
  String CHECK_PENDING_ASSIGNMENTS = "/checkPendingAssignments";
  String BULK_REVIEW_UPLOAD = "/{processType}/bulkReviewUpload";
}
