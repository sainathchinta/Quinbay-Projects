package com.gdn.mta.product.web.model;

public class SolrReindexControllerPath {
  private static final String PRD_COLLECTION = "/prd-collection";
  private static final String DELETE = "/delete";
  private static final String SYNC_INACTIVE = "/sync/inactive";
  public static final String BASE_PATH = "/api/solr/reindex";
  public static final String MOVE_EXISTING_SOLR_COLLECTION_TO_CLOUD = "/moveToSolrCloud";
  public static final String PRD_COLLECTION_DELETE = PRD_COLLECTION + DELETE;
  public static final String PRD_COLLECTION_SYNC_INACTIVE = PRD_COLLECTION + SYNC_INACTIVE;
  public static final String REINDEX_IN_REVIEW_PRODUCTS = PRD_COLLECTION + "/reindexInReviewProducts";
  public static final String REINDEX_IN_REVIEW_PRODUCT_BY_PRODUCT_CODE = PRD_COLLECTION + "/reindexInReviewProductByProductCode";
  public static final String REINDEX_ACTIVE_PRODUCT_BY_PRODUCT_CODE = PRD_COLLECTION + "/reindexActiveProductByProductCode";
  public static final String REINDEX_VARIANT_HISTORY = PRD_COLLECTION + "/reindexVariantHistory";
  public static final String DELTA_REINDEX_VARIANT_HISTORY = PRD_COLLECTION + "/historyDeltaReindex";
  public static final String PRD_COLLECTION_DELTA_REINDEX = PRD_COLLECTION + "/deltaReindex";

  private SolrReindexControllerPath() {}
}
