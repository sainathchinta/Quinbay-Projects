package com.gdn.partners.product.analytics.model;

public interface AutoApprovedApiPath {
  String BASE_PATH = "/api/auto-approved";
  String AUTO_APPROVED_PRODUCTS_LIST_API = "/products-list";
  String UPSERT_USER_FEEDBACK = "/{productCode}/upsert-user-feedback";
  String UPDATE_ASSIGNEE = "/update-assignee";
  String GET_USER_FEEDBACK_FOR_AUTO_APPROVED_PRODUCTS = "/{productCode}/user-feedback-for-auto-approved-products";
  String AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD = "/selected-download";
}
