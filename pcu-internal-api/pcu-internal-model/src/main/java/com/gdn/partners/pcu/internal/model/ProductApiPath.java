package com.gdn.partners.pcu.internal.model;

/**
 * Created by govind on 10/01/2019 AD.
 */
public interface ProductApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/products";
  String REVISION_REASONS = "/get-revision-reasons";
  String RETURN_FOR_CORRECTION = "/return-for-correction";
  String MERGE = "/merge";
  String UPDATE = "/update";
  String UPDATE_PRODUCT_ASSIGNMENT = "/update-product-assignment";
  String DETAIL = "/{productCode}/detail";
  String FILTER_COUNTS = "/filterCounts";
  String REVIEW_PRODUCTS_FILTER = "/getScreeningProducts/filter";
  String APPROVE_DRAFT = "/approveDraft";
  String BULK_SCREENING_PRODUCT_ACTIONS = "/bulk-screening-product-actions/{action}";
  String PRODUCT_HISTORY = "/{productId}/history";
  String SCREENING_SUGGESTION = "/screening-suggestion";
  String SCREENING_PRODUCT_SEARCH = "/screening-product-search";
  String GET_REVIEWER_FOR_PRODUCT = "/reviewer";
  String GET_PRODUCT_REVISION_HISTORY = "/{productCode}/revision-history";
  String DELETE_REVIEWER_FOR_PRODUCT = "/delete-reviewer";
  String CATEGORY_CHANGE_CHECK = "/category-change-check/{productCode}";
  String GET_ALL_PRODUCTS = "/getAllProducts";
  String PRODUCTS_SUSPENSION = "/doSuspensionAction";
  String SUSPENSION_HISTORY = "/getSuspensionHistory";
  String BULK_PRODUCT_SUSPENSION = "/doBulkSuspensionAction";
  String UPDATE_PRODUCT_CATEGORY = "/{productCode}/update-product-category";
  String GET_ACTIVE_PRODUCTS = "/filter/keyword";
  String BULK_UPDATE_MASTER_PRODUCT = "/bulkUpdateMasterProduct";
  String FILTER_COUNTS_BY_SOURCE = "/filterCountsBySource";
  String DOWNLOAD_SELECTED_MASTER_PRODUCTS = "/download/selected";
  String DOWNLOAD_MASTER_PRODUCTS = "/download/all";
  String GET_PRODUCT_HISTORY = "/history";
  String RETRY_PRODUCT_PUBLISH_TO_PDT = "/retryProductPublishToPDT";
  String REINDEX_BY_PRODUCT_SKU = "/reindex/{productSku}";
  String RETRY_PRODUCT_NEED_REVISION_TO_PBP = "/retryProductNeedRevisionToPBP";
  String CHECK_CATEGORY_CHANGE = "/{productCode}/check-change-category";
  String REINDEX_BY_PRODUCT_CODE = "/reindexProductCode/{productCode}";

  String GET_ALL_INTERNAL_TEMPLATE_DOWNLOAD_PATHS = "/getAllInternalTemplateDownloadPaths";
  String GET_HAHAL_PRODUCT_DETAILS_BY_PRODUCT_SKU = "/get-halal-product-details-by-productSku/{productSku}";
  String GET_HALAL_CERTIFICATION_DETAILS = "/get-halal-certification-details/{certificationNumber}";
  String GET_HALAL_DASHBOARD_PRODUCTS = "/get-halal-products-listing";
  String GET_HALAL_PRODUCT_HISTORY = "/{productSku}/get-halal-product-history-by-productSku";
  String UPDATE_PRODUCT_HALAL_CONFIG = "/{productSku}/update-curation-status";
}
