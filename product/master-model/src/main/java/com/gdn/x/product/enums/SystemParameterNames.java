package com.gdn.x.product.enums;

public class SystemParameterNames {

  public static final String HANDLING_FEE = "product_system_parameter_handling_fee";
  public static final String SUBJECT_ORDER_RECEIVED_DEFAULT = "subject_order_received_default";
  public static final String SUBJECT_ORDER_PAID_DEFAULT = "subject_order_paid_default";
  public static final String BARCODE_SUBJECT_DEFAULT = "barcode_subject_default";
  public static final String EMAIL_SENDER_DEFAULT = "email_sender_default";
  public static final String SOLR_REINDEX_SIZE = "solr_reindex_size";
  public static final String SOLR_REINDEX_BATCH_SIZE_L3 = "solr_reindex_batch_size_l3";
  public static final String REINDEX_XPRODUCT_L3_COLLECTION = "reindex_xproduct_l3_collection";
  public static final String MIGRATION_LOCATION_PATH = "migration_location_path";
  public static final String MASTER_DATA_CONCURRENT_SIZE = "master_data_concurrent_size";
  public static final String SOLR_COMMIT_WITHIN = "solr_commit_within";
  public static final String PRODUCT_AVAILIBILITY_REQUEST_LIMIT =
      "product_availibility_request_limit";
  public static final String PRISTINE_SETTINGS_ENABLED = "pristine_settings_enabled";
  public static final String PRISTINE_SETTINGS_TESTING_MODE_ENABLED = "pristine_settings_testing_mode_enabled";
  public static final String PRISTINE_ID_DATA_LOOKUP_ENABLED = "pristine_id_data_lookup_enabled";
  public static final String MINIMUM_PRICE = "minimum_price";
  public static final String RETRY_PUBLISH_BATCH_SIZE = "retry_publish_batch_size";
  public static final String X_PROMO_API_SWITCH = "system_parameter_x_promo_api_switch";
  public static final String LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT =
      "promo_adjustment_change_event_listen_switch";
  public static final String FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED = "fetch_items_by_product_sku_enabled";
  public static final String FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED = "fetch_products_by_product_code_enabled";
  public static final String PUBLISH_OFFLINE_ITEMS_OFFSET = "publish_offline_items_offset";
  public static final String REPUBLISH_OFFLINE_ITEMS_OFFSET = "republish_offline_items_offset";
  public static final String PUBLISH_OFFLINE_ITEMS_RUNNING_STATE = "publish_offline_items_running_state";
  public static final String REPUBLISH_OFFLINE_ITEMS_RUNNING_STATE = "republish_offline_items_running_state";
  public static final String LIMIT_OFFLINE_ITEMS = "limit_offline_items";
  public static final String SOLR_LAST_INDEX_TIME = "last_reindex_time";
  public static final String SOLR_LAST_INDEX_TIME_L3 = "last_reindex_time_l3";
  public static final String SOLR_ITEM_LAST_INDEX_TIME = "item_last_reindex_time";
  public static final String SOLR_MERCHANT_VOUCHER_ITEMS_PAGE_SIZE = "merchant_voucher_items_page_size";
  public static final String ENABLE_DEFERRED_SOLR_REINDEX = "enable_deferred_solr_reindex";
  public static final String ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX = "enable_offline_deferred_solr_reindex";
  public static final String PROCESS_DEFERRED_SOLR_REINDEX = "process_deferred_solr_reindex";
  public static final String PROCESS_ATOMIC_SOLR_REINDEX = "process_atomic_solr_reindex";
  public static final String DEFERRED_SOLR_REINDEX_PAGE_SIZE = "deferred_solr_reindex_page_size";
  public static final String PRODUCT_SKU_LIST_SIZE = "product_sku_list_size";
  public static final String ITEM_SKU_LIST_SIZE = "item_sku_list_size";
  public static final String DEFERRED_SOLR_REINDEX_EVENT_SIZE = "deferred_solr_reindex_event_size";
  public static final String DELETE_FROM_SOLR_EVENT_SIZE = "delete_from_solr_event_size";
  public static final String YOUTUBE_URL_VALIDATION_SWITCH = "youTubeUrlValidationSwitch";
  public static final String VARIANT_CREATION_MADE_NON_MANDATORY = "variant_creation_made_non_mandatory";
  public static final String VARIANT_CREATION_MANDATORY_SCORE = "variant_creation_mandatory_score";
  public static final String SLEEP_TIME_FOR_REINDEX = "sleep_time_for_reindex";
  public static final String SOLR_REINDEX_BATCH_SIZE_L4 = "solr_reindex_batch_size_l4";
  public static final String SOLR_REINDEX_SIZE_PRISTINE_ITEM = "solr_reindex_size_pristine_item";
  public static final String DISABLE_REINDEX_JOB_L3_SOLR = "disable_reindex_l3_collection";
  public static final String REINDEX_L3_QUERY_REINDEX_COUNT_OVERRIDE =
      "reindex_l3_collection_max_count";
  public static final String REINDEX_L3_QUERY_PAGE_SIZE = "reindex_l3_collection_page_size";
  public static final String REINDEX_L3_EVENT_PAYLOAD_SIZE = "reindex_l3_collection_payload_size";
  public static final String REINDEX_L3_WAIT_TIME_BETWEEN_BATCHES =
      "reindex_l3_collection_wait_time";
  public static final String INVENTORY_BATCH_SIZE = "inventory_l3_request_batch_size";
  public static final String REINDEX_L3_WAIT_TIME_BETWEEN_ITERATIONS =
      "reindex_l3_collection_iteration_wait_time";
  public static final String SOLR_CATEGORY_LIST_BATCH_SIZE = "solr_category_list_batch_size";
  public static final String SINGLE_ITEM_SUMMARY_SOLR_SWITCH = "singleItemSummarySolrSwitch";
  public static final String REVERT_TRANSACTION_API_SWITCH = "revertTransactionApiSwitch";
  public static final String MAX_ITEM_COUNT_TRANSACTION_API_SWITCH = "maxItemCountTransactionApiSwitch";
  public static final String MASTER_DATA_CONTROL_SWITCH = "masterDataFetchSwitch";
  public static final String SELLER_PROMO_BUNDLINGS_SWITCH = "sellerPromoBundlingsSwitch";
  public static final String L3_REINDEX_ENABLED_IN_FULL_REINDEX_API_SWITCH = "l3ReindexEnabledInFullReindexApiSwitch";

  public static final String MIGRATION_MAX_ITEMS_TO_PUBLISH = "migrationMaxItemsToPublish";
  public static final String MIGRATION_PAGE_SIZE_FOR_ITEMS = "migrationPageSizeForItems";
  public static final String MIGRATION_EVENT_PAYLOAD_SIZE = "migrationEventPayloadSize";
  public static final String MIGRATION_EVENT_PUBLISH_WAIT_TIME = "migrationEventPublishWaitTime";
  public static final String MIGRATION_PAGE_FETCH_WAIT_TIME = "migrationPageFetchWaitTime";

  public static final String ITEM_CHANGE_SWITCH = "itemChangeListenerSwitch";
  public static final String OFFLINE_ITEM_CHANGE_SWITCH = "offlineItemChangeListenerSwitch";
  public static final String MIGRATION_ITEM_CREATION_CHANGE_SWITCH = "migrationItemCreationSwitch";
  public static final String PRODUCT_SKU_LIST_LIMIT = "productSkuListLimit";
  public static final String PRODUCT_SKU_LIST_LIMIT_FOR_BASIC_DETAILS = "productSkuListLimitBasicDetails";
  public static final String PAGE_SIZE_TO_UPDATE_L5_DURING_ARCHIVAL = "pageSizeToUpdateL5DuringArchival";

  public static final String BATCH_SIZE_TO_DELETE_ARCHIVED_PRODUCTS = "archivedProductDeletionBatchSize";
  public static final String DAYS_THRESHOLD_FOR_ARCHIVED_PRODUCT_DELETION = "daysThresholdForArchivedProductDeletion";
  public static final String CONTINUE_PUBLISHING_DELTA_REINDEX_EVENT = "continuePublishingDeltaReindexEvent";
  public static final String EAN_UPC_FETCH_MAX_LIMIT = "eanUpcFetchMaxLimit";
}
