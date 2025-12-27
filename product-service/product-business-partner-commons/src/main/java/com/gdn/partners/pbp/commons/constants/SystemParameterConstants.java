package com.gdn.partners.pbp.commons.constants;

public interface SystemParameterConstants {

  String PRODUCT_IMAGE_QC_ENABLED = "productImageQcEnable";
  String PRODUCT_IMAGE_QC_SYNC = "productImageQcSync";
  String PRODUCT_IMAGE_QC_CATEGORY_LIST = "productImageQcCategoryList";
  String CATEGORY_CODE_ALL = "all";
  String OFFLINE_PRODUCT_HISTORY_PROHIBITED_MERCHANT_CODE =
      "offlineProductHistoryProhibitedMerchantCode";
  String OFFLINE_PRODUCT_HISTORY_X_PRODUCT_INTEGRATOR_CLIENT_ID =
      "offlineProductHistoryXProductIntegratorClientId";
  String PRODUCT_MIGRATION_RETRY_BATCH_SIZE = "productMigrationRetryBatchSize";
  String PRODUCT_MIGRATION_RETRY_COUNT = "productMigrationRetryCount";
  String ITEM_SUMMARY_PAGE_SIZE = "itemSummaryPageSize";
  String CHECK_PRODUCT_FOR_REVIEW = "checkProductForReview";
  String HISTORY_REINDEXING_PAGE_SIZE = "historyReindexingPageSize";
  String AUDIT_TRAIL_DELETE_BATCH_SIZE = "auditTrailDeleteBatchSize";
  String AUDIT_TRAIL_DELETE_BATCH_SIZE_SOLR = "auditTrailDeleteBatchSizeSolr";
  String VALID_IMAGE_EXTENSION = "validImageExtension";
  String UPDATE_OFFLINE_FEATURE_ON = "updateOfflineItemStockEnabled";
  String AVOID_INVENTORY_UPDATE_FLAG = "avoidInventoryUpdateFlag";
  String IMAGE_BUNDLE_SWITCH = "imageBundleSwitch";
  String REJECTED_API_SWITCH = "rejectedApiSwitch";
  String SAP_OUTBOUND_SWITCH = "sapOutBoundSwitch";
  String MAX_LIMIT_L3_RETRY = "maxL3RetryLimit";
  String THRESHOLD_LIMIT_MAILER = "thresholdMailerLimit";
  String RESIGN_SELLER_SWITCH = "resignSellerSwitch";
  String SAP_CALL_ENABLED = "sapCallEnabled";
  String MAX_STOCK_LIMIT = "maxStockLimit";
  String MAX_VARIANTS = "maxVariants";
  String MAX_ASSIGNEE_LIMIT = "maxAssigneeLimit";
  String MAX_AUTO_ASSIGNMENT_PRODUCT_COUNT = "maxAutoAssignmentProductCount";
  String PRODUCT_FETCH_BATCH_SIZE = "productFetchBatchSize";
  String BULK_LISTING_INTERVAL_LIMIT = "bulkListingIntervalLimit";
  String BULK_ESTIMATION_INTERVAL_LIMIT = "bulkEstimationIntervalLimit";
  String SIZE_CHART_FEATURE_SWITCH_PHASE_1 = "sizeChartPhase1";
  String SIZE_CHART_FEATURE_SWITCH_PHASE_2 = "sizeChartPhase2";
  String BOPIS_CATEGORY_RESTRICTION_FEATURE_SWITCH = "bopisCategoryRestrictionFeatureSwitch";
  String UPDATE_HISTORY_DELETE_BATCH_SIZE = "updateHistoryDeleteBatchSize";
  String UPDATE_HISTORY_DELETE_PAGES_SIZE = "updateHistoryDeletePagesSize";
  String BRAND_TAKE_DOWN_THRESHOLD = "brandTakeDownThreshold";
  String SYNC_PRODUCTS_BEFORE_MIN = "syncProductsBeforeMin";
  String SYNC_PRODUCTS_DIFFERENCE_MIN = "syncProductsDifferenceMin";

  String SYNC_ACTIVE_PRODUCTS_BEFORE_MIN = "syncActiveProductsBeforeMin";
  String SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN = "syncActiveProductsDifferenceMin";

  String SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN = "syncPreLiveProductsBeforeMin";
  String SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN = "syncPreLiveProductsDifferenceMin";

  String IS_REVISED_PUBLISH_ENABLED = "revisedPublishedEnabled";
  String USE_PCB_SWITCH = "usePCB";
  String DELTA_REINDEX_HOUR_THRESHOLD = "deltaReindexHourThreshold";
  String DELTA_REINDEX_BATCH_SIZE = "deltaReindexBatchSize";

  String backFillFbbFlagFetchSize = "backFillFbbFlagFetchSize";
  String backFillFbbFlagBatchSize = "backFillFbbFlagBatchSize";
  String pickupPointChangeFetchSize = "pickupPointChangeFetchSize";
  String pickupPointChangeBatchSize = "pickupPointChangeBatchSize";
  String LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION = "lastDeltaReindexTimeForPrdProductCollection";
  String FETCH_ADD_DELETE_VARIANT_PENDING_STATUS_IN_HOURS = "fetchAddDeleteVariantPendingStatusInBeforeHours";
  String FETCH_ADD_DELETE_VARIANT_PENDING_STATUS_BATCH_SIZE = "fetchAddDeleteVariantPendingStatusBatchsize";
  String APPEAL_PRODUCT_LIMIT = "appealProductLimit";
  String BOPIS_UNSUPPORTED_MERCHANT_TYPES = "bopisUnsupportedMerchantTypes";

  String BOPIS_CNC_RESTRICTION_FEATURE_SWITCH = "bopisCNCRestrictionFeatureSwitch";
  String WAREHOUSE_STOCK_VALIDATION_SUPPORTED_MERCHANT_TYPES = "validateWarehouseDeletionEligibleSellers";
  String VALIDATE_WAREHOUSE_VARIANT_DELETION_FEATURE_SWITCH = "validateWarehouseVariantDeletionEnabled";
  String PRE_ORDER_FEATURE_SWITCH_FOR_UI = "preOrderSwitch";
}