package com.gdn.x.mta.distributiontask.rest.model;

public interface ScheduledJobControllerPath {

  String BASE_PATH = "/api/scheduled-jobs";
  String RUN_POST_LIVE_CONFIG_CHANGES = "/run-vendor-post-live-config-changes";
  String AUTO_APPROVAL_OF_PENDING_PRODUCTS = "/auto-approval-of-pending-product";
  String RETRY_PRODUCTS_BY_ACTION = "/retry-products-by-action";
  String Add_PRODUCT_TO_AUTO_APPROVAL = "/add-product-to-auto-approval";
  String PUBLISH_PENDING_COMMON_IMAGE_RECORDS = "/{migrationType}/publish-pending-migration-records";
  String SYNC_NEED_CORRECTION_PRODUCTS = "/sync-need-correction-products";
  String SEND_EVIDENCE_REQUESTED_MAIL = "/send-evidence-requested-mail";
}
