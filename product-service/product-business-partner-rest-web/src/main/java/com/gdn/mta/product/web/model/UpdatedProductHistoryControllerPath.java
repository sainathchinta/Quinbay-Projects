package com.gdn.mta.product.web.model;

public class UpdatedProductHistoryControllerPath {
  
  public static final String BASE_PATH = "/api/audit-trial";
  public static final String SAVE = "/api/save";
  public static final String GET_PRD_UPD_AUDITLOGS = "/product-update-logs";
  public static final String CHECK_PRODUCT_PRICE_CHANGE = "/{sku}/is-price-changed";
  public static final String CHECK_PRODUCT_PRICE_CHANGE_FOR_L5 = "/{sku}/{pickupPointCode}/is-price-changed";
  public static final String GET_OFFLINE_PRD_UPD_AUDIT_LOGS = "/offline-item-update-logs";
  public static final String DELETE_FROM_DB = "/delete-from-Db";
  public static final String DELETE_FROM_SOLR = "/delete-from-solr";
}
