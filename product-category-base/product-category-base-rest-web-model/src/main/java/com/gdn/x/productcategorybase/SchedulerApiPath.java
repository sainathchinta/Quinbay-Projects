package com.gdn.x.productcategorybase;

public interface SchedulerApiPath {
  String BASE_PATH = "/api/scheduler";
  String BACKFILL_PRODUCT_ATTRIBUTE_EXTRACTED = "/backfill/product-attribute-extracted";
  String PUBLISH_PENDING_COMMON_IMAGE_RECORDS = "/{migrationType}/publish-pending-migration-records";
  String DELETE_ARCHIVED_PRODUCT_DATA = "/deleteArchivedProductData";
  String PUBLISH_REJECTED_PRODUCTS_FOR_DELETION = "/publishRejectedProductForDeletion";
  String ACTIVATE_BRAND_AUTHORISATION = "/activate-brand-authorisation";
  String UPDATE_PRODUCT_MIGRATION_STATUS = "/update-product-migration-status";
  String SEND_NEAR_EXPIRY_MAIL = "/send-near-expiry-mail";
}
