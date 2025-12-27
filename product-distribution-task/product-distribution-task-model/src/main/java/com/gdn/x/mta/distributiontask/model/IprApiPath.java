package com.gdn.x.mta.distributiontask.model;

public interface IprApiPath {
  String BASE_PATH = "/api/ipr";
  String SUSPENSION_IN_PROGRESS = "/suspension-in-progress";
  String FILTER = "/filter/summary";
  String SUBMIT_EVIDENCE = "/submit-evidence";
  String ADD_PRODUCT_IPR = "/add-product/{productSku}";
  String UPDATE_ASSIGNEE = "/update-assignee";
  String DETAILS = "/details/{productSku}";
  String PERFORM_IPR_ACTION = "/action";
  String PRIMARY_FILTER_COUNTS = "/primary-filter-counts";
  String SUSPEND_EVIDENCE_REQUESTED_PRODUCT = "/suspended-evidence-requested-product";
  String HISTORY = "/history";
}
