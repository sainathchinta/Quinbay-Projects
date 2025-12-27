package com.gdn.mta.product.web.model;

public interface ScheduledJobControllerPath {

  String BASE_PATH = "/api/scheduled-jobs";
  String RUN_POST_LIVE_CONFIG_CHANGES = "/run-post-live-config-changes";
  String MIGRATE_PRODUCTS = "/migrate-products";
  String RETRY_MIGRATE_PRODUCTS = "/retry-migrate-product";
  String UPDATE_MIGRATION_STATUS = "/update-migration-status";
  String PUBLISH_IMAGE_QC_BACKLOG_PRODUCTS = "/publish-image-qc-backlog-products";
  String SYNC_IN_REVIEW_PRODUCTS = "/sync-in-review-products";
  String SYNC_ACTIVE_PRODUCTS = "/sync-active-products";
  String SYNC_PRE_LIVE_PRODUCTS = "/sync-pre-live-products";
  String MIGRATE_FBB_PRODUCTS = "/migrate-fbb-products/{migrationType}";
  String ADD_DELETE_VARIANT_RETRY_PUBLISH = "/add-delete-variant-retry-publish";
}
