package com.gdn.partners.pcu.internal.model;

public interface BrandWipApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/brand-wip";
  String FILTER_SUMMARY = "/filter/summary";
  String DETAIL = "/{brandRequestCode}";
  String HISTORY_SUMMARY = "/history/filter/summary";
  String APPROVE = "/{brandRequestCode}/approve";
  String REJECT = "/{brandRequestCode}/reject";
  String REJECTION_REASON = "/{brandRequestCode}/rejectionReason";
  String UPDATE = "/update";
}
