package com.gdn.partners.bulk.util;

public interface RecatConstants {

  // Recat Status
  String NEW = "NEW";
  String IN_PROGRESS = "IN_PROGRESS";
  String FINISHED = "FINISHED";
  String FAILED = "FAILED";
  String PARTIAL_SUCCESS = "PARTIAL_SUCCESS";
  String CANCELLED = "CANCELLED";

  // Product Status
  String PENDING = "PENDING";
  String PUBLISHED = "PUBLISHED";

  String SYSTEM_ERROR = "SYSTEM_ERROR";
  String VALIDATION_ERROR = "VALIDATION_ERROR";

  //Excel columns
  String PRODUCT_CODE = "ProductCode";
  String PRODUCT_NAME = "ProductName";
  String MASTER_CATEGORY_CODE = "MasterCategoryCode";
  String MASTER_CATEGORY_NAME = "MasterCategoryName";
  String NEW_CATEGORY_CODE = "NewMasterCategoryCode";
  String NEW_CATEGORY_NAME = "NewMasterCategoryName";

  //System parameter config

  // Scheduler - 1
  String PROCESS_NEW_REQUESTS = "recatProcessNewRequests";

  // Scheduler - 2
  String PUBLISH_PENDING_PRODUCTS = "recatProcessPendingProducts";

  // Scheduler - 2 fetch limit for pending products
  String PENDING_PRODUCTS_FETCH_LIMIT = "recatProcessPendingProductsFetchLimit";

  // Number of products to be saved at once to blp_product_recat_status table
  String SAVE_RECAT_PROCESS_BATCH_SIZE = "recatProcessSaveDBBatchSize";

  // Scheduler - 2 Number of products to be published in each event
  String PENDING_PRODUCTS_PUBLISH_BATCH_SIZE = "recatProcessPublishProductsBatchSize";

  //Scheduler - 3
  String CHECK_PENDING_PRODUCTS = "recatProcessUpdateFinalStatus";

  //Published products move to failed state minutes threshold
  String PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD = "recatProcessPublishedProductsThreshold";

  //Error messages
  String FILE_IS_EMPTY = "File is empty, request cancelled";

  String FIELD_STORE_ID = "storeId";
  String FIELD_RECAT_REQUEST_CODE = "recatRequestCode";
  String FIELD_STATUS = "status";
  String FIELD_PRODUCT_CODE = "productCode";
  String FIELD_PRODUCT_NAME = "productName";
  String FIELD_CATEGORY_CODE = "categoryCode";
  String FIELD_NEW_CATEGORY_CODE = "newCategoryCode";
  String FIELD_UPDATED_DATE = "updatedDate";
  String ALL_STATUS = "ALL";
  String SUCCEED_NOTE = "Succeed";
  String IN_PROGRESS_NOTE = "In Progress";

  String PRODUCT_CODE_EMPTY = "Product code is empty";
  String PRODUCT_NAME_EMPTY = "Product name is empty";
  String CATEGORY_CODE_EMPTY = "Category code is empty";
  String CATEGORY_NAME_EMPTY = "Category name is empty";
  String NEW_CATEGORY_CODE_EMPTY = "New Category code is empty";
  String NEW_CATEGORY_NAME_EMPTY = "New Category Name is empty";
  String RECAT_PROCESS_NOT_FOUND = "Recat request not found";
  String RECAT_PROCESS_INVALID_STATE = "Recat request is not in NEW status";

  String CANCELLED_BY_USER = "Cancelled by User - ";
  String CANCELLED_BY_SYSTEM = "Cancelled by System";
  String VALIDATION_ERROR_MESSAGE = "Can not process invalid input data :";
  String RECAT_BATCH_SIZE = "recatBatchSize";
}