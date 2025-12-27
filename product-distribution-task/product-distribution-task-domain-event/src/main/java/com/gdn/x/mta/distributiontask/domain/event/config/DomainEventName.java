package com.gdn.x.mta.distributiontask.domain.event.config;

public class DomainEventName {
  public static final String PRODUCT_SCREENING_APPROVED_TASK_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.screening.approved";

  public static final String PRODUCT_QC_APPROVED_TASK_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.qc.approved";

  public static final String PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.vendor.approved";

  public static final String PRODUCT_VENDOR_APPROVED_TASK_PRIORITY_1_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.vendor.approved.priority.1";

  public static final String PRODUCT_VENDOR_APPROVED_TASK_PRIORITY_2_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.vendor.approved.priority.2";

  public static final String EDITED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.edited.product.vendor.approved";

  public static final String REVISED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.revised.product.vendor.approved";

  public static final String PRODUCT_REVISED_TASK_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.revised";

  public static final String PRODUCT_REJECTED_TASK_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.rejected";
  
  public static final String PRODUCT_FORCE_CLOSE_TASK_EVENT_AND_ROLLBACK_PRODUCT_NAME =
      "com.gdn.x.mta.distributiontask.product.forcecloserollback";

  public static final String PRODUCT_FINAL_APPROVED_TASK_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.approved";

  public static final String PDT_PRODUCT_SOLR_ADD_DOCUMENT_BATCH_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.batch.add";

  public static final String PDT_PRODUCT_SOLR_DELETE_DOCUMENT_BATCH_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.batch.delete";

  public static final String PDT_PRODUCT_AUTO_APPROVAL_CRITERIA_CHECK_EVENT_NAME =
      "com.gdn.x.mta.distributiontask.product.auto.approval.criteria.check";

  public static final String PDT_PRODUCT_AUTO_APPROVAL_EVENT =
      "com.gdn.x.mta.distributiontask.product.auto.approval.event";

  public static final String PDT_PRODUCT_UPDATE_TO_SOLR_EVENT_NAME =
      "com.gdn.x.product.distributiontask.product.update.to.solr";

  public static final String SELLER_AUTO_QC_DATA_UPDATE = "com.gdn.product.analytics.seller.auto.qc.data.update";

  public static final String PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING =
      "com.gdn.x.product.distributiontask.product.item.common.image.backfilling";
  public static final String PRODUCT_DATA_AUTO_FIX_HISTORY = "com.gdn.pbp.product.data.auto.fix.history";

  public static final String PRODUCT_AND_ITEM_IMAGE_PATH_UPDATE_EVENT = "com.gdn.x.productcategorybase.product.item.image.path.update";

  public static final String DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT = "com.gdn.x.mta.distributiontask.product.delete.original.images";

  public static final String PDT_PRODUCT_COMBINED_UPDATE_TO_SOLR_EVENT_NAME = "com.gdn.x.product.distributiontask.product.combined.update.to.solr";

  public static final String VENDOR_SEARCH_AUTO_HEAL_EVENT_NAME = "com.gdn.x.product.distributiontask.vendor.search.auto.heal";

  public static final String PDT_PRODUCT_HISTORY_EVENT = "com.gdn.x.product.distributiontask.product.history.event";

}
