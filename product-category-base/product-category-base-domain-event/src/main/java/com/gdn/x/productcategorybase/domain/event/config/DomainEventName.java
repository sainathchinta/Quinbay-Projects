package com.gdn.x.productcategorybase.domain.event.config;

public class DomainEventName {

  public static final String PRODUCT_PUBLISH_ALL = "com.gdn.x.productcategorybase.product.publish.all";
  public static final String PRODUCT_CATEGORY_PUBLISH_ALL = "com.gdn.x.productcategorybase.product.category.publish.all";
  public static final String PRODUCT_ATTRIBUTE_PUBLISH_ALL = "com.gdn.x.productcategorybase.product.attribute.publish.all";
  public static final String IMAGE_PUBLISH_ALL = "com.gdn.x.productcategorybase.image.publish.all";
  public static final String PRODUCT_ITEM_PUBLISH_ALL = "com.gdn.x.productcategorybase.product.item.publish.all";
  public static final String PRODUCT_PUBLISH = "com.gdn.x.productcategorybase.product.publish";
  public static final String CATEGORY_PUBLISH = "com.gdn.x.productcategorybase.category.publish";
  public static final String ALL_CATEGORY_PUBLISH = "com.gdn.x.productcategorybase.all.category.publish";
  public static final String PRODUCT_EVENT = "com.gdn.x.productcategorybase.product.event";
  public static final String BRAND_UPDATED = "com.gdn.x.productcategorybase.brand.updated";
  public static final String BRAND_CREATED = "com.gdn.x.productcategorybase.brand.created";
  public static final String BRAND_DELETED = "com.gdn.x.productcategorybase.brand.deleted";
  public static final String SOLR_ADD_BRAND_EVENT = "com.gdn.x.productcategorybase.brand.add.to.solr";
  public static final String SOLR_DELETE_BRAND_EVENT = "com.gdn.x.productcategorybase.brand.delete.from.solr";
  public static final String SOLR_UPDATE_BRAND_EVENT = "com.gdn.x.productcategorybase.brand.update.to.solr";
  public static final String SOLR_ADD_PCB_PRODUCT_EVENT = "com.gdn.x.productcategorybase.pcb.product.solr.add";
  public static final String SOLR_DELETE_PCB_PRODUCT_EVENT = "com.gdn.x.productcategorybase.pcb.product.solr.delete";
  public static final String BRAND_APPROVED_OR_REJECTED_EVENT = "com.gdn.x.productcategorybase.brand.approved.or.rejected.event";
  public static final String SOLR_ADD_BATCH_PCB_PRODUCT_EVENT = "com.gdn.x.productcategorybase.pcb.product.solr.add.batch";
  public static final String SOLR_DELETE_BATCH_PCB_PRODUCT_EVENT = "com.gdn.x.productcategorybase.pcb.product.solr.delete.batch";
  public static final String MASTER_ATTRIBUTE_INFO_EVENT = "com.gdn.x.productcategorybase.attribute.publish";
  public static final String PRODUCT_SCORE_UPDATE_EVENT_NAME = "com.gdn.x.productcategorybase.product.score";
  public static final String PRODUCT_CREATION_FAILURE = "com.gdn.catalog.product.creation.failure";
  public static final String VAT_UPDATE_PUBLISH = "com.gdn.x.productcategorybase.vat.applicable.update";
  public static final String VAT_UPDATE_EXTERNAL_HISTORY_PUBLISH = "com.gdn.x.productcategorybase.vat.applicable.update.history";
  public static final String PRODUCT_ATTRIBUTE_EXTRACTION_BACKFILLING_PUBLISH =
      "com.gdn.x.productcategorybase.product.attribute.extraction.backfilling";
  public static final String PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING =
      "com.gdn.x.productcategorybase.product.item.common.image.backfilling";
  public static final String PRODUCT_AND_ITEM_GFS_TO_GCS_MIGRATION =
      "com.gdn.x.productcategorybase.product.item.gfs.to.gcs.migration";
  public static final String PRODUCT_AND_ITEM_GFS_TO_GCS_FINAL_IMAGE_MIGRATION =
      "com.gdn.x.productcategorybase.product.item.gfs.to.gcs.final.image.migration";
  public static final String PRODUCT_MASTER_DATA_MIGRATION = "com.gdn.x.productcategorybase.product.publish.migration";
  public static final String BRAND_AUTH_HISTORY_EVENT =
    "com.gdn.x.productcategorybase.brand.auth.history.event";
  public static final String RESTRICTED_KEYWORD_HISTORY_EVENT = "com.gdn.pcb.restricted.keyword.history.event";
  public static final String DELETE_REJECTED_PRODUCT = "com.gdn.x.productcategorybase.delete.rejected.product";
  public static final String DELETE_REJECTED_MERCHANT_PRODUCT = "com.gdn.x.productcategorybase.delete.rejected.merchant.product";
  public static final String UPDATE_PRODUCT_ITEM_IMAGE_PATH = "com.gdn.x.productcategorybase.product.item.image.path.update";
  public static final String PRODUCT_ATTRIBUTE_BACKFILLING =
    "com.gdn.x.productcategorybase.product.attribute.backfilling";
}
