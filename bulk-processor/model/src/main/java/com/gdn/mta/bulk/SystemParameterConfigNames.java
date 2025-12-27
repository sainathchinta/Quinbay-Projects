package com.gdn.mta.bulk;

import org.springframework.context.annotation.Configuration;

@Configuration
public interface SystemParameterConfigNames {

  String DISABLE_BULK_UPDATE_VARIABLE = "disable_bulk_updates";
  String DISABLE_BULK_CNC_UPDATES_VARIABLE = "disable_bulk_cnc_updates";
  String BRAND_SCHEDULER_RUN_TIME = "brand_sheet_regenerate_time";
  String CATEGORY_ATTRIBUTE_SCHEDULER_LAST_RUN_TIMESTAMP = "category_attribute_scheduler_lastRun_timeStamp";
  String BRAND_SHEET_APPENDING_INREVIEW_BRAND_FLAG = "brand_sheet_appending_inreview_brand_flag";
  String CATEGORY_BRAND_SCHEDULER_RUN_TIME = "category_brand_sheet_regenerate_time";
  String VENDOR_BULK_DOWNLOAD_NEW_SIZE = "vendor_bulk_download_new_size";
  String VENDOR_BULK_DOWNLOAD_MAX_LIMIT = "vendor_bulk_download_max_limit";
  String BULK_UPDATE_OFF2ON_BATCH_SIZE = "bulk_update_off2on_batch_size";
  String BULK_ARCHIVE_PRODUCT_SKUS_BATCH_SIZE = "bulk_archive_batch_size";
  String BULK_VAT_UPDATE_BATCH_SIZE = "bulk_vat_update_batch_size";
  String BULK_VAT_UPDATE_SWITCH = "bulk_vat_update_switch";
  String PRICING_API_BATCH_SIZE = "pricing_api_batch_size";
  String DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE = "dormant_seller_product_fetch_batch_size";
  String SUSPEND_SELLER_PRODUCT_FETCH_BATCH_SIZE = "suspend_seller_product_fetch_batch_size";
  String TERMINATED_SELLER_PRODUCT_FETCH_BATCH_SIZE = "terminated_seller_product_fetch_batch_size";
  String DORMANT_SELLER_PRODUCT_UPDATE_MAX_SIZE = "dormant_seller_product_update_max_size";
  String SUSPEND_SELLER_PRODUCT_UPDATE_MAX_SIZE = "suspend_seller_product_update_max_size";
  String TERMINATED_SELLER_PRODUCT_UPDATE_MAX_SIZE = "terminated_seller_product_update_max_size";
  String DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE = "dormant_seller_event_fetch_batch_size";
  String SUSPEND_SELLER_EVENT_FETCH_BATCH_SIZE = "suspend_seller_event_fetch_batch_size";
  String PRODUCT_PARTITION_SIZE = "product_partition_size";

  String BULK_PROCESSED_BATCH_SIZE = "bulk_processed_batch_size";
  String BULK_CREATION_PROCESS_ORDER_BY = "bulk_creation_order_by";
  String DELETE_DAY_RANGE = "delete_day_range";
  String STUCK_PROCESS_CHECK_TIME = "stuck_process_check_time";
  String ABORT_PENDING_BULK_PROCESS_LIMIT = "abort_pending_bulk_process_limit";
  String UNMAPPED_SKU_FETCH_BATCH_SIZE = "unmapped_sku_fetch_batch_size";

  //publishing batch size
  String PROCESS_DATA_ARCHIVE_BATCH_SIZE = "process_data_archive_batch_size";
  String PROCESS_DATA_VAT_UPDATE_BATCH_SIZE = "process_data_vat_update_batch_size";
  String PROCESS_DATA_UPDATE_BATCH_SIZE = "process_data_update_batch_size";
  String PROCESS_DATA_UPSERT_BATCH_SIZE = "process_data_upsert_batch_size";
  String PROCESS_DATA_DELETE_BATCH_SIZE = "process_data_delete_batch_size";
  String PROCESS_DATA_CAMPAIGN_BATCH_SIZE = "process_data_campaign_batch_size";
  String PROCESS_DATA_INSTORE_BATCH_SIZE = "process_data_instore_batch_size";
  String QR_GENERATION_BATCH_SIZE = "qr_generation_batch_size";
  String PROCESS_DATA_SUSPENSION_BATCH_SIZE = "process_data_suspension_batch_size";

  //publishing batch size
  String BULK_READY_TO_PROCESS_BATCH_SIZE = "bulk_ready_to_process_batch_size";
  String BULK_READY_TO_PROCESS_ARCHIVE_BATCH_SIZE = "bulk_ready_to_process_archive_batch_size";
  String BULK_READY_TO_PROCESS_UPDATE_BATCH_SIZE = "bulk_ready_to_process_update_batch_size";
  String BULK_READY_TO_PROCESS_UPSERT_BATCH_SIZE = "bulk_ready_to_process_upsert_batch_size";
  String BULK_READY_TO_PROCESS_DELETE_BATCH_SIZE = "bulk_ready_to_process_delete_batch_size";
  String BULK_READY_TO_PROCESS_CAMPAIGN_BATCH_SIZE = "bulk_ready_to_process_campaign_batch_size";
  String BULK_READY_TO_PROCESS_INSTORE_BATCH_SIZE = "bulk_ready_to_process_instore_batch_size";
  String BULK_READY_TO_PROCESS_VAT_UPDATE_BATCH_SIZE = "bulk_ready_to_process_vat_update_batch_size";
  //Store Copy Config
  String STORE_COPY_PRODUCT_FETCH_BATCH_SIZE = "store_copy_product_fetch_batch_size";
  String STORE_COPY_ABORT_PENDING_TASK_IN_MINUTES = "store_copy_abort_pending_task_in_minutes";
  String STORE_COPY_FAIL_PENDING_TASK_IN_MINUTES = "store_copy_fail_pending_task_in_minutes";
  String STORE_COPY_ABORT_IN_PROGRESS_DOWNLOADS_BULK_DOWNLOAD_ENTITY =
      "store_copy_abort_in_progress_downloads_bulk_download_entity";
  String STORE_COPY_FILE_BATCH_SIZE = "store_copy_file_batch_size";
  String STORE_COPY_FILE_TOTAL_BATCH_SIZE = "store_copy_file_total_batch_size";
  String STORE_COPY_STATUS_UPDATE_BATCH_SIZE = "store-copy-status-update-batch-size";
  String STORE_COPY_DELETE_BATCH_SIZE = "store_copy_delete_batch_size";
  String STORE_COPY_DELETE_DAYS_BEFORE = "store_copy_delete_days_before";

  //Internal bulk upload
  String INTERNAL_BLUK_UPLOAD_FILE_BATCH_SIZE = "internal_bulk_upload_file_batch_size";
  String INTERNAL_BLUK_UPLOAD_FILE_TOTAL_BATCH_SIZE = "internal_bulk_upload_file_total_batch_size";
  String INTERNAL_BULK_UPLOAD_FETCH_BATCH_SIZE = "internal_bulk_upload_fetch_batch_size";
  String INTERNAL_BULK_UPLOAD_DELETE_BATCH_SIZE = "internal_bulk_upload_delete_batch_size";
  String INTERNAL_BULK_UPLOAD_DELETE_DAYS_BEFORE = "internal_bulk_upload_delete_days_before";
  String INTERNAL_BULK_UPLOAD_ABORT_PENDING_TASK_IN_MINUTES = "internal_bulk_upload_abort_pending_task_in_minutes";
  String INTERNAL_BULK_UPLOAD_FAIL_PENDING_TASK_IN_MINUTES = "internal_bulk_upload_fail_pending_task_in_minutes";
  String INTERNAL_BULK_UPLOAD_STATUS_UPDATE_BATCH_SIZE = "internal-bulk-upload-status-update-batch-size";

  // Vendor Bulk Assignment
  String VENDOR_BULK_ASSIGNMENT_FILE_BATCH_SIZE = "vendor_bulk_assignment_file_batch_size";
  String VENDOR_BULK_ASSIGNMENT_FILE_TOTAL_BATCH_SIZE = "vendor_bulk_assignment_file_total_batch_size";
  String VENDOR_BULK_ASSIGNMENT_FETCH_BATCH_SIZE = "vendor_bulk_assignment_fetch_batch_size";
  String VENDOR_BULK_ASSIGNMENT_DELETE_BATCH_SIZE = "vendor_bulk_assignment_delete_batch_size";
  String VENDOR_BULK_ASSIGNMENT_DELETE_DAYS_BEFORE = "vendor_bulk_assignment_delete_days_before";
  String VENDOR_AUTO_ASSIGNMENT_DELETE_DAYS_BEFORE = "vendor_auto_assignment_delete_days_before";
  String VENDOR_BULK_ASSIGNMENT_ABORT_PENDING_TASK_IN_MINUTES = "vendor_bulk_assignment_abort_pending_task_in_minutes";
  String VENDOR_AUTO_ASSIGNMENT_ABORT_PENDING_TASK_IN_MINUTES = "vendor_auto_assignment_abort_pending_task_in_minutes";
  String VENDOR_BULK_ASSIGNMENT_FAIL_PENDING_TASK_IN_MINUTES = "vendor_bulk_assignment_fail_pending_task_in_minutes";
  String VENDOR_BULK_ASSIGNMENT_STATUS_UPDATE_BATCH_SIZE = "vendor_bulk_assignment_status_update_batch_size";
  String VENDOR_AUTO_ASSIGNMENT_STATUS_UPDATE_BATCH_SIZE = "vendor_auto_assignment_status_update_batch_size";

  //Update Sales Catgeory Config
  String UPDATE_SALES_CATEGORY_FILE_BATCH_SIZE = "update_sales_category_file_batch_size";
  String UPDATE_SALES_CATEGORY_FILE_TOTAL_BATCH_SIZE = "update_sales_category_file_total_batch_size";
  String UPDATE_SALES_CATEGORY_FETCH_BATCH_SIZE = "update_sales_category_fetch_batch_size";
  String UPDATE_SALES_CATEGORY_DELETE_BATCH_SIZE = "update_sales_category_delete_batch_size";
  String UPDATE_SALES_CATEGORY_DELETE_DAYS_BEFORE = "update_sales_category_delete_days_before";
  String UPDATE_SALES_CATEGORY_ABORT_PENDING_TASK_IN_MINUTES = "update_sales_category_abort_pending_task_in_minutes";
  String UPDATE_SALES_CATEGORY_FAIL_PENDING_TASK_IN_MINUTES = "update_sales_category_fail_pending_task_in_minutes";
  String UPDATE_SALES_CATEGORY_STATUS_UPDATE_BATCH_SIZE = "update-sales-category-status-update-batch-size";

  String SUSPENSION_FILE_BATCH_SIZE = "suspension_file_batch_size";
  String SUSPENSION_DELETE_BATCH_SIZE = "suspension_delete_batch_size";
  String SUSPENSION_DELETE_DAYS_BEFORE = "suspension_delete_days_before";
  String SUSPENSION_ABORT_PENDING_TASK_IN_MINUTES = "suspension_abort_pending_task_in_minutes";
  String SUSPENSION_FAIL_PENDING_TASK_IN_MINUTES = "suspension_fail_pending_task_in_minutes";

  //new upload workflow config
  String BULK_SWITCH = "newBulkWorkflowSwitch";
  String BULK_UPDATE_SWITCH = "newBulkUpdateWorkflowSwitch";
  String CAMPAIGN_UPLOAD_SWITCH = "newCampaignUploadWorkflowSwitch";
  String PICKUP_UPSERT_SWITCH = "newPickupUpsertWorkflowSwitch";
  String PICKUP_DELETE_SWITCH = "newPickupDeleteWorkflowSwitch";
  //Instore new upload flow switch
  String INSTORE_BULK_UPDATE_SWITCH = "instore_bulk_update_switch";

  //Business partner terminated switch
  String BUSINESS_PARTNER_TERMINATION_SWITCH = "business-partner-termination-switch";
  String TERMINATED_SELLER_EVENT_FETCH_BATCH_SIZE = "terminated_seller_event_fetch_batch_size";

  String BULK_ARCHIVE_IMPLEMENTATION = "bulk_archive_implementation";
  String DOWNLOAD_PROCESS_BATCH_SIZE = "download_process_batch_size";

  // new configuration update params
  String CONFIG_UPDATE_FILE_BATCH_SIZE = "config_update_file_batch_size";
  String CONFIG_UPDATE_DELETE_BATCH_SIZE = "config_update_delete_batch_size";
  String CONFIG_UPDATE_DELETE_DAYS_BEFORE = "config_update_delete_days_before";
  String CONFIG_UPDATE_ABORT_PENDING_TASK_IN_MINUTES = "config_update_abort_pending_task_in_minutes";
  String CONFIG_UPDATE_FAIL_PENDING_TASK_IN_MINUTES = "config_update_fail_pending_task_in_minutes";
  String PROCESS_DATA_CONFIG_UPDATE_BATCH_SIZE = "process_data_config_update_batch_size";

  String BULK_GENERIC_UPLOAD_MAXIMUM_SIZE = "bulk_generic_upload_maximum_size";
  String BULK_UPLOAD_PRODUCT_MAXIMUM_SIZE = "bulk_upload_product_maximum_size";
  String PICKUP_POINT_BATCH_SIZE = "pickup_point_batch_size";

  String BULK_ORDER_DOWNLOAD_PAGE_SIZE = "bulk_order_download_page_size";
  String BULK_ORDER_DOWNLOAD_SOLR_ENABLED = "bulk_order_download_solr_enabled";
  String BULK_ORDER_DOWNLOAD_SCAN_AND_GO_CHANNELS = "bulk_order_download_offline_scan_and_go_channels";

  String BULK_ARCHIVE_UPLOAD_MAXIMUM_SIZE = "bulk_archive_upload_maximum_size";
  String DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE = "dormant_seller_status_update_batch_size";

  // delete brand auth
  String DELETE_BRAND_AUTH_FILE_BATCH_SIZE = "delete_brand_auth_file_batch_size";
  String DELETE_BRAND_AUTH_FILE_TOTAL_BATCH_SIZE = "delete_brand_auth_file_total_batch_size";
  String DELETE_BRAND_AUTH_FETCH_BATCH_SIZE = "delete_brand_auth_fetch_batch_size";
  String DELETE_BRAND_AUTH_STATUS_UPDATE_BATCH_SIZE = "config_update_file_batch_size";

  //priority queue system param
  String FETCH_PENDING_DELETE_PICKUP_POINT_BATCH_SIZE = "fetch_pending_delete_pickup_point_size";
  String PRIORITY_QUEUE_ENABLED = "priority_queue_enabled";
  String UPDATE_PRIORITY_QUEUE_ENABLED = "update_priority_queue_enabled";
  String TRUSTED_SELLER_MAX_ROW_SIZE = "trusted_seller_maximum_row_size";
  String REGULAR_SELLER_MAX_ROW_SIZE = "regular_seller_maximum_row_size";
  String REGULAR_SELLER_MIN_ROW_SIZE = "regular_seller_minimum_row_size";
  String UPDATE_TRUSTED_SELLER_MAX_ROW_SIZE = "update_trusted_seller_maximum_row_size";
  String UPDATE_REGULAR_SELLER_MAX_ROW_SIZE = "update_regular_seller_maximum_row_size";
  String UPDATE_REGULAR_SELLER_MIN_ROW_SIZE = "update_regular_seller_minimum_row_size";
  String PICKUP_POINT_ORDER_DOWNLOAD_BATCH_SIZE = "pickup_point_order_download_batch_size";
  String BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_PRIORITY_1 =
    "bulk_ready_to_process_batch_size_priority_1";
  String BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_PRIORITY_2 =
    "bulk_ready_to_process_batch_size_priority_2";
  String BULK_READY_TO_PROCESS_UPDATE_PRIORITY_1_BATCH_SIZE =
      "bulk_ready_to_process_update_batch_size_priority_1";
  String BULK_READY_TO_PROCESS_UPDATE_PRIORITY_2_BATCH_SIZE =
      "bulk_ready_to_process_update_batch_size_priority_2";
  String BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_CONVERTED_UPLOAD =
    "bulk_ready_to_process_batch_size_converted_upload";
  String BULK_READY_TO_PROCESS_BATCH_SIZE_FOR_EXTERNAL_UPLOAD =
    "bulk_ready_to_process_batch_size_external_upload";

  //FBB_L5_CREATE_BATCH_SIZE
  String FETCH_PENDING_FBB_L5_SIZE = "fetch_pending_fbb_l5_create_size";
  String FETCH_PENDING_FBB_L4_ROW_SIZE = "fbb_l4_batch_size";
  String L4_FETCH_BATCH_SIZE = "l4_fetch_batch_size";
  String FBB_L4_RESULT_UPDATE_BATCH_SIZE = "fbb_l4_result_update_batch_size";
  String VENDOR_AUTO_ASSIGNMENT_FILE_TOTAL_BATCH_SIZE =
    "vendor_auto_assignment_file_total_batch_size";
  String VENDOR_AUTO_ASSIGNMENT_FILE_BATCH_SIZE = "vendor_auto_assignment_file_batch_size";
  String VENDOR_AUTO_ASSIGNMENT_FETCH_BATCH_SIZE = "vendor_auto_assignment_fetch_batch_size";
  String VENDOR_AUTO_ASSIGNMENT_FAIL_PENDING_TASK_IN_MINUTES = "vendor_auto_assignment_fail_pending_task_in_minutes";
  String VENDOR_AUTO_ASSIGNMENT_DELETE_BATCH_SIZE = "vendor_auto_assignment_delete_batch_size";

  //Restricted keyword update batch size
  String RESTRICTED_KEYWORD_UPSERT_FINAL_UPDATE_BATCH_SIZE = "restricted_keyword_upsert_final_update_batch_size";
  String RESTRICTED_KEYWORD_DELETE_FINAL_UPDATE_BATCH_SIZE = "restricted_keyword_delete_final_update_batch_size";
  String RESTRICTED_KEYWORD_BULK_UPSERT_TOTAL_BATCH_SIZE = "restricted_keyword_bulk_upsert_total_batch_size";
  String RESTRICTED_KEYWORD_BULK_DELETE_TOTAL_BATCH_SIZE = "restricted_keyword_bulk_delete_total_batch_size";
  String RESTRICTED_KEYWORD_BULK_UPSERT_FILE_BATCH_SIZE = "restricted_keyword_bulk_upsert_file_batch_size";
  String RESTRICTED_KEYWORD_BULK_DELETE_FILE_BATCH_SIZE = "restricted_keyword_bulk_delete_file_batch_size";
  String RESTRICTED_KEYWORD_BULK_UPSERT_FETCH_BATCH_SIZE = "restricted_keyword_bulk_upsert_fetch_batch_size";
  String RESTRICTED_KEYWORD_BULK_DELETE_FETCH_BATCH_SIZE = "restricted_keyword_bulk_delete_fetch_batch_size";
  String RESTRICTED_KEYWORD_BULK_UPSERT_FAIL_BEFORE = "restricted_keyword_bulk_upsert_fail_before";
  String RESTRICTED_KEYWORD_BULK_DELETE_FAIL_BEFORE = "restricted_keyword_bulk_delete_fail_before";
  String RESTRICTED_KEYWORD_BULK_UPSERT_ABORT_BEFORE = "restricted_keyword_bulk_upsert_abort_before";
  String RESTRICTED_KEYWORD_BULK_DELETE_ABORT_BEFORE = "restricted_keyword_bulk_delete_abort_before";
  String BULK_APPROVAL_ABORT_BEFORE_IN_MINUTES = "bulk_approval_abort_before_in_minutes";
  String BULK_REJECTION_ABORT_BEFORE_IN_MINUTES = "bulk_rejection_abort_before_in_minutes";
  String BULK_APPROVAL_FAIL_BEFORE_IN_MINUTES = "bulk_approval_fail_before_in_minutes";
  String BULK_REJECTION_FAIL_BEFORE_IN_MINUTES = "bulk_rejection_fail_before_in_minutes";
  String RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BATCH_SIZE = "restricted_keyword_bulk_upsert_data_delete_batch_size";
  String RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BATCH_SIZE = "restricted_keyword_bulk_delete_data_delete_batch_size";
  String RESTRICTED_KEYWORD_BULK_UPSERT_DATA_DELETE_BEFORE = "restricted_keyword_bulk_upsert_data_delete_before";
  String RESTRICTED_KEYWORD_BULK_DELETE_DATA_DELETE_BEFORE = "restricted_keyword_bulk_delete_data_delete_before";

  //Brand Auth Configurations
  String BRAND_AUTH_ADD_FILE_BATCH_SIZE = "brand_auth_bulk_add_file_batch_size";
  String BRAND_AUTH_DELETE_FILE_BATCH_SIZE = "brand_auth_bulk_delete_file_batch_size";
  String BRAND_AUTH_BULK_ADD_TOTAL_BATCH_SIZE = "brand_auth_bulk_add_total_batch_size";
  String BRAND_AUTH_BULK_DELETE_TOTAL_BATCH_SIZE = "brand_auth_bulk_delete_total_batch_size";
  String BRAND_AUTH_BULK_ADD_FETCH_BATCH_SIZE = "brand_auth_bulk_add_fetch_batch_size";
  String BRAND_AUTH_BULK_DELETE_FETCH_BATCH_SIZE = "brand_auth_bulk_delete_fetch_batch_size";
  String BRAND_AUTH_ADD_FINAL_UPDATE_BATCH_SIZE = "brand_auth_bulk_add_final_update_batch_size";
  String BRAND_AUTH_DELETE_FINAL_UPDATE_BATCH_SIZE = "brand_auth_bulk_delete_final_update_batch_size" ;

  //BULK APPROVAL REJECTION SIZE CONFIG
  String BULK_APPROVAL_DB_BATCH_SIZE = "bulk_approval_db_batch_size";
  String BULK_REJECTION_DB_BATCH_SIZE = "bulk_rejection_db_batch_size";
  String BULK_APPROVAL_ROW_BATCH_SIZE = "bulk_approval_row_batch_size";
  String BULK_REJECTION_ROW_BATCH_SIZE = "bulk_rejection_row_batch_size";
  String BULK_APPROVAL_FETCH_BATCH_SIZE = "bulk_approval_fetch_batch_size";
  String BULK_REJECTION_FETCH_BATCH_SIZE = "bulk_rejection_fetch_batch_size";
  String BULK_VENDOR_APPROVAL_FINAL_UPDATE_SIZE = "bulk_vendor_approval_final_update_batch_size";
  String BULK_VENDOR_REJECTION_FINAL_UPDATE_SIZE = "bulk_vendor_rejection_final_update_batch_size";

  String BULK_QR_UPLOAD_MAXIMUM_LIMIT = "bulk_qr_upload_maximum_size";
  String MAX_QR_ALLOWED_BY_PARTNER = "max_qr_allowed_by_partner";
  String FETCH_PENDING_QRCODE_BATCH_SIZE = "fetch_pending_qrcode_size";

  // Brand Update
  String BRAND_UPDATE_TOTAL_BATCH_SIZE = "brand_update_total_batch_size";
  String BRAND_UPDATE_ROW_BATCH_SIZE = "brand_update_row_batch_size";
  String BRAND_UPDATE_FETCH_BATCH_SIZE = "brand_update_fetch_batch_size";
  String BULK_WORK_ORDER_UPLOAD_BATCH_SIZE = "bulk_work_order_upload_batch_size";

  //Master Sku
  String MASTER_SKU_BULK_ASSIGNEE_FAIL_BEFORE_IN_MINUTES =
    "master_sku_bulk_assignee_fail_before_in_minutes";
  String MASTER_SKU_BULK_REVIEW_FAIL_BEFORE_IN_MINUTES =
    "master_sku_bulk_review_fail_before_in_minutes";
  String MASTER_SKU_BULK_ASSIGNEE_ROW_BATCH_SIZE = "master_sku_bulk_assignee_row_batch_size";
  String MASTER_SKU_BULK_ASSIGNEE_DB_BATCH_SIZE = "master_sku_bulk_assignee_db_batch_size";
  String MASTER_SKU_BULK_ASSIGNEE_FETCH_BATCH_SIZE = "master_sku_bulk_assignee_fetch_batch_size";
  String BULK_MASTER_SKU_ASSIGNEE_FINAL_UPDATE_SIZE =
    "bulk_master_sku_assignee_final_update_batch_size";
  String FETCH_PENDING_MASTER_SKU_REVIEW_BATCH_SIZE = "pending_master_sku_review_batch_size";
  String MASTER_SKU_REVIEW_DB_BATCH_SIZE = "master_sku_review_db_batch_size";
  String MASTER_SKU_REVIEW_ROW_BATCH_SIZE = "master_sku_bulk_review_row_batch_size";
  String BULK_MASTER_SKU_REVIEW_FINAL_UPDATE_SIZE = "bulk_master_sku_review_final_update_batch_size";

  //Auto approved products
  String AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_ROW_BATCH_SIZE =
    "auto_approved_products_bulk_assign_row_batch_size";
  String AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_DB_BATCH_SIZE =
    "auto_approved_products_bulk_assign_db_batch_size";
  String AUTO_APPROVED_PRODUCTS_BULK_ASSIGN_FETCH_BATCH_SIZE =
    "auto_approved_products_bulk_assignee_fetch_batch_size";
  String AUTO_APPROVED_BULK_ASSIGNEE_FAIL_BEFORE_IN_MINUTES =
    "auto_approved_bulk_assignee_fail_before_in_minutes";
  String AUTO_APPROVED_BULK_ASSIGNEE_FINAL_UPDATE_SIZE =
    "auto_approved_bulk_assignee_final_update_batch_size";

  //Price update tool
  String INTERNAL_PRICE_UPDATE_ROW_BATCH_SIZE = "internal_price_update_row_batch_size";
  String INTERNAL_PRICE_UPDATE_DB_BATCH_SIZE = "internal_price_update_db_batch_size";
  String INTERNAL_PRICE_UPDATE_PER_PARENT_PUBLISH_BATCH_SIZE = "internal_price_update_per_parent_batch_size";
  String INTERNAL_PRICE_UPDATE_FETCH_BATCH_SIZE = "internal_price_update_db_fetch_batch_size";
  String INTERNAL_PRICE_UPDATE_FINAL_UPDATE_SIZE = "internal_price_update_final_update_size";

  String PRODUCT_LEVEL3_UPDATE_PRIORITY1_ABORT_STRUCK_PROCESS_IN_MINUTES = "product_level3_update_priority1_abort_struck_process_in_minutes";
  String PRODUCT_LEVEL3_UPDATE_PRIORITY2_ABORT_STRUCK_PROCESS_IN_MINUTES = "product_level3_update_priority2_abort_struck_process_in_minutes";
  String PRODUCT_LEVEL3_ABORT_STRUCK_PROCESS_IN_MINUTES = "product_level3_priority2_abort_struck_process_in_minutes";
  String PRODUCT_CREATION_UPLOAD_PRIORITY_ABORT_STRUCK_PROCESS_IN_MINUTES = "product_creation_upload_priority_abort_struck_process_in_minutes";
  String PRODUCT_CREATION_UPLOAD_PRIORITY_2_ABORT_STRUCK_PROCESS_IN_MINUTES = "product_creation_upload_priority_2_abort_struck_process_in_minutes";
  String CONVERTED_PRODUCT_CREATION_UPLOAD_ABORT_STRUCK_PROCESS_IN_MINUTES = "converted_product_creation_upload_abort_struck_process_in_minutes";
  String PRODUCT_CREATION_UPLOAD_PRIORITY_1_ABORT_STRUCK_PROCESS_IN_MINUTES =
    "product_creation_upload_priority_1_abort_struck_process_in_minutes";
  String INSTANT_PICKUP_PRODUCT_DELETE_ABORT_STRUCK_PROCESS_IN_MINUTES =
    "instant_pickup_product_delete_abort_struck_process_in_minutes";
  String INSTANT_PICKUP_PRODUCT_UPSERT_ABORT_STRUCK_PROCESS_IN_MINUTES =
    "instant_pickup_product_upsert_abort_struck_process_in_minutes";
  String IN_STORE_ABORT_STRUCK_PROCESS_IN_MINUTES = "in_store_abort_struck_process_in_minutes";

  String ARCHIVE_ABORT_STRUCK_PROCESS_IN_MINUTES = "archive_abort_struck_process_in_minutes";
  String ASSEMBLY_REQUEST_STRUCK_PROCESS_IN_MINUTES = "assembly_request_struck_process_in_minutes";
  String TRANSFER_REQUEST_STRUCK_PROCESS_IN_MINUTES = "assembly_request_struck_process_in_minutes";
  String DISASSEMBLY_REQUEST_STRUCK_PROCESS_IN_MINUTES =
    "disassembly_request_struck_process_in_minutes";
  String DELETE_PICKUP_POINT_STRUCK_PROCESS_IN_MINUTES =
    "disassembly_request_struck_process_in_minutes";
  String PRODUCT_CREATION_UPLOAD_STRUCK_PROCESS_IN_MINUTES =
    "product_creation_upload_struck_process_in_minutes";

  String PRODUCT_LEVEL_3_GENERIC_STRUCK_PROCESS_IN_MINUTES =
    "product_level_3_generic_struck_process_in_minutes";
  String CAMPAIGN_STRUCK_PROCESS_IN_MINUTES = "campaign_struck_process_in_minutes";
  String SUBJECT_TO_VAT_PROCESS_IN_MINUTES = "subject_to_vat_struck_process_in_minutes";

  //Bulk Price Rebate
  String BULK_PRICE_REBATE_SIZE = "br_price_rebate_update_file_fetch_size";
  String BULK_PRICE_REBATE_TOTAL_BATCH_SIZE= "bulk_rebate_total_batch_size";
  String PRICE_REBATE_FETCH_BATCH_SIZE = "bulk_price_rebate_db_fetch_batch_size";
  String REBATE_UPDATE_FINAL_UPDATE_SIZE = "rebate_update_final_update_size";

  String BULK_PRODUCT_TYPE_TAGGING_ROW_BATCH_SIZE = "br_product_type_tagging_file_fetch_size";
  String BULK_PRODUCT_TYPE_TAGGING_UPDATE_DB_BATCH_SIZE =
    "bulk_product_type_tagging_update_db_batch_size";
  String BULK_PRODUCT_TYPE_TAGGING_UPDATE_FETCH_BATCH_SIZE = "bulk_product_type_tagging_update_fetch_batch_size";
  String BULK_PRODUCT_TYPE_TAGGING_UPDATE_PUBLISH_BATCH_SIZE = "bulk_product_type_tagging_update_publish_batch_size";
  String BULK_DELETE_PICKUP_POINT_READY_TO_PROCESS_BATCH_SIZE = "bulk_delete_pickup_point_ready_to_process_batch_size";

  String BULK_PRODUCT_TYPE_TAGGING_UPDATE_FINAL_UPDATE_SIZE = "bulk_product_type_tagging_update_final_update_size";
  String PRODUCT_TYPE_TAGGING_PUBLISH_BATCH_SIZE = "product_type_tagging_publish_batch_size";
  String FETCH_BATCH_SIZE_FOR_PUBLISHED_STRUCK_PROCESSES =
    "fetch_batch_size_for_published_struck_processes";

  // Bulk Sku Level Rebate
  String BULK_SKU_LEVEL_REBATE_ROW_BATCH_SIZE = "bulk_sku_level_rebate_row_batch_size";
  String BULK_SKU_LEVEL_REBATE_TOTAL_BATCH_SIZE= "bulk_sku_level_rebate_total_batch_size";
  String BULK_SKU_LEVEL_REBATE_FETCH_BATCH_SIZE = "bulk_sku_level_rebate_fetch_batch_size";
  String BULK_SKU_LEVEL_REBATE_FINAL_UPDATE_SIZE = "bulk_sku_level_rebate_final_update_size";
  String IPR_PRODUCTS_ADD_REVIEW_ROW_BATCH_SIZE = "ipr_products_bulk_add_review_row_batch_size";
  String IPR_PRODUCTS_ADD_REVIEW_DB_BATCH_SIZE = "ipr_products_bulk_add_review_db_batch_size";
  String IPR_PRODUCTS_ADD_REVIEW_FETCH_BATCH_SIZE = "ipr_products_bulk_add_review_fetch_batch_size";
  String IPR_PRODUCTS_ADD_REVIEW_FAIL_BEFORE_IN_MINUTES =
      "ipr_products_add_review_fail_before_in_minutes";
  String IPR_PRODUCTS_ADD_REVIEW_FINAL_UPDATE_SIZE = "ipr_products_add_review_final_update_size";
  String IPR_ACTIONS = "ipr_actions";
  String IPR_SOURCE = "ipr_source";
  String IPR_VIOLATION_TYPES = "ipr_violation_types";
  String IPR_REASONS = "ipr_reasons";

  // Bulk New Price Update
  String BULK_NEW_PRICE_UPDATE_ROW_BATCH_SIZE = "bulk_new_price_update_row_batch_size";
  String BULK_NEW_PRICE_UPDATE_TOTAL_BATCH_SIZE= "bulk_new_price_update_total_batch_size";
  String BULK_NEW_PRICE_UPDATE_FETCH_BATCH_SIZE = "bulk_new_price_update_fetch_batch_size";
  String BULK_NEW_PRICE_UPDATE_FINAL_UPDATE_SIZE = "bulk_new_price_update_final_update_size";

  //NR Deletion SysParams
  String NR_DELETION_FETCH_PRODUCTS_WITH_BP_CODES_SIZE = "nr_deletion_fetch_products_with_bp_codes_size";

  // Dormant seller retry time threshold
  String DORMANT_SELLER_RETRY_THRESHOLD_TIME_IN_MINUTES = "dormant_seller_retry_threshold_in_minutes";
  String DORMANT_SELLER_NOTIFY_THRESHOLD_TIME_IN_MINUTES = "dormant_seller_notify_threshold_in_minutes";

  // Bulk basic info update
  String BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE = "bulk_upload_product_basic_info_maximum_size";


  // batch size for Basic Info Update
  String PROCESS_DATA_BASIC_INFO_UPDATE_BATCH_SIZE = "process_data_basic_info_update_batch_size";
  String BULK_READY_TO_PROCESS_BASIC_INFO_BATCH_SIZE = "bulk_ready_to_process_basic_info_batch_size";
  String BULK_READY_TO_PROCESS_BASIC_INFO_PRIORITY_1_BATCH_SIZE = "bulk_ready_to_process_basic_info_priority_1_batch_size";
  String BULK_READY_TO_PROCESS_BASIC_INFO_PRIORITY_2_BATCH_SIZE = "bulk_ready_to_process_basic_info_priority_2_batch_size";

  //internal brand update
  String INTERNAL_BRAND_UPDATE_BATCH_SIZE = "internal_brand_update_batch_size";
  String INTERNAL_BRAND_UPDATE_DATA_BATCH_SIZE = "internal_brand_update_data_batch_size";

  //internal brand name update
  String INTERNAL_BRAND_NAME_UPDATE_BATCH_SIZE = "internal_brand_name_update_batch_size";
  String INTERNAL_BRAND_NAME_UPDATE_DATA_BATCH_SIZE = "internal_brand_name_update_data_batch_size";
  String EXTERNAL_MARKETPLACE_SHEET_RULES = "external_marketplace_sheet_rules";

}
