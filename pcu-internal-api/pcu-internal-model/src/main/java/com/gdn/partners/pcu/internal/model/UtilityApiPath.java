package com.gdn.partners.pcu.internal.model;

public interface UtilityApiPath {

  String BASE_PATH = Constants.CONTEXT_PATH + "/api/utility";
  String PUBLISH_ADD_EDITED_EVENT_TO_PDT = "/{productCode}/publish-add-edited-event-to-PDT";
  String PUBLISH_ADD_REVISED_EVENT_TO_PDT = "/{productCode}/publish-add-revised-event-to-PDT";
  String RETRY_PRODUCT_IMAGE_RESIZE = "/{productCode}/retry-product-image-resize";
  String RETRY_EDITED_PRODUCT_IMAGE_RESIZE = "/{productCode}/retry-edited-product-image-resize";
  String UPDATE_PBP_PRODUCT_WORKFLOW = "/{productCode}/update-pbp-product-workflow";
  String UPDATE_PBP_REVIEW_PENDING = "/{productCode}/update-pbp-review-pending";
  String UPDATE_PBP_ACTIVATED_AND_VIEWABLE = "/{productCode}/update-pbp-activated-and-viewable";
  String CHECK_AUTO_APPROVAL_ELIGIBILITY = "/{productCode}/check-auto-approval-eligibility";
  String REPUBLISH_PCB_PRODUCT_PUBLISH_EVENT = "/{productCode}/republish-pcb-product-publish-event";
  String CLEAR_PCB_PRODUCT_CACHE = "/{productCode}/clear-pcb-product-cache";
  String UPDATE_PCB_PRODUCT_VIEWABLE = "/{productCode}/update-pcb-product-viewable";
  String UPDATE_PCB_PRODUCT_REVIEW_PENDING = "/{productCode}/update-pcb-product-review-pending";
  String UPDATE_PDT_STATE_UPDATE = "/{productCode}/update-product-status";
  String TAKE_DOWN_OR_REACTIVE_PRODUCT_IN_X_PRODUCT = "/{productSku}/take-down-or-reactivate-product";
  String ABORT_PENDING_BULK_PROCESS_DATA_BY_ID = "/abort-pending-tasks";
  String ABORT_PENDING_DOWNLOADS_BY_ENTITY = "/abort-pending-downloads-by-entity";

  String DELETE_PRODUCT_BY_PRODUCT_CODE = "/delete-product-collection";
  String REINDEX_ACTIVE_PRODUCT_BY_PRODUCT_CODE = "/{productCode}/reindexActiveProductByProductCode";
  String FETCH_DATA_FROM_BIG_QUERY_FOR_MASTER_SKU_REVIEW = "/fetchDataFromBigQuery";
  String MIGRATE_PRODUCT_AND_L5_DETAIL_BY_PRODUCT_SKU = "/migrateProductAndL5DetailsByProductSku";
  String UPDATE_SYSTEM_PARAMETER_IN_PCB = "/updateSystemParameterInPCB";
  String GENERATE_PRODUCT_SCORE = "/generateProductScore";
  String PUBLISH_PRODUCT_ATTRIBUTE_EXTRACTIONS = "/publish-product-attribute-extractions";
}
