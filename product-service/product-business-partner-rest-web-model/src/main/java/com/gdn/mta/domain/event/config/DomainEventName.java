package com.gdn.mta.domain.event.config;

public interface DomainEventName {
  String PRODUCT_APPROVE_IMAGE_REQUEST = "com.gdn.mta.request.approve.image";
  String PRODUCT_APPROVE_IMAGE_RESPONSE = "com.gdn.mta.response.approve.image";
  String PRODUCT_STATUS_UPDATE_TRACKER = "com.gdn.mta.product.approval.detail.status.update";
  String PRODUCT_PROCESS_IMAGE_REQUEST = "com.gdn.mta.request.product.process.image";
  String PRODUCT_DELETE_EVENT_REQUEST = "com.gdn.pdt.product.delete";
  String CATEGORY_TO_BUSINESS_PARTNER_MAPPING_SAVE_EVENT = "com.gdn.mta.save.category.business.partner.mapping";
  String CATEGORY_TO_PRODUCT_CODE_MAPPING_SAVE_EVENT = "com.gdn.mta.save.category.product.code.mapping";
  String CATEGORY_TO_PRODUCT_CODE_MAPPING_EVENT = "com.gdn.mta.bulk.category.to.product.code.mapping";
  String CATEGORY_TO_PRODUCT_SKU_MAPPING_SAVE_EVENT = "com.gdn.mta.save.category.product.sku.mapping";
  String PRODUCT_SKU_TO_SALES_CATALOG_SAVE_EVENT = "com.gdn.mta.save.product.sku.to.sales.catalog.mapping";
  String SOLR_PRODUCT_COLLECTION_UPDATE = "com.gdn.mta.product.collection.update.into.solr";
  String PRODUCT_MINIMUM_STOCK_NOTIFICATION = "com.gdn.pbp.product.minimum.stock.notification";
  String PRODUCT_OOS_STOCK_NOTIFICATION = "com.gdn.pbp.product.oos.stock.notification";
  String SOLR_ADD_REVIEW_PRODUCT_REQUEST = "com.gdn.pbp.solr.add.review.product";
  String SOLR_DELETE_REVIEW_PRODUCT_REQUEST = "com.gdn.pbp.solr.delete.review.product";
  String PRODUCT_MAIL_EVENT_SEND_FOR_CORRECTION = "com.gdn.pbp.send.product.mail.send.for.correction";
  String PRODUCT_MAIL_EVENT_SEND_FOR_CORRECTION_EN = "com.gdn.pbp.send.product.mail.send.for.correction.en";
  String PRODUCT_MAIL_EVENT_REJECTED_PRODUCT = "com.gdn.pbp.send.product.mail.rejected";
  String PRODUCT_MAIL_EVENT_REJECTED_PRODUCT_EN = "com.gdn.pbp.send.product.mail.rejected.en";
  String PRODUCT_MAIL_EVENT_APPROVED_PRODUCT = "com.gdn.pbp.send.product.mail.approved";
  String PRODUCT_MAIL_EVENT_APPROVED_PRODUCT_EN = "com.gdn.pbp.send.product.mail.approved.en";
  String PRODUCT_MAIL_EVENT_CATEGORY_CHANGE = "com.gdn.pbp.send.product.category.change";
  String PRODUCT_MAIL_EVENT_CATEGORY_CHANGE_EN = "com.gdn.pbp.send.product.category.change.en";
  String PRODUCT_MAIL_EVENT_AUTO_ARCHIVE_ITEM_SKU = "com.gdn.pbp.send.auto.archive.mail";
  String PRODUCT_MAIL_EVENT_AUTO_ARCHIVE_ITEM_SKU_EN = "com.gdn.pbp.send.auto.archive.mail.en";
  String PRODUCT_MAIL_EVENT_OOS_ITEM_SKU = "com.gdn.pbp.send.oos.mail";
  String PRODUCT_MAIL_EVENT_OOS_ITEM_SKU_EN = "com.gdn.pbp.send.oos.mail.en";
  String PRODUCT_MAIL_EVENT_MIN_STOCK_ITEM_SKU = "com.gdn.pbp.send.minimum.stock.mail";
  String PRODUCT_MAIL_EVENT_MIN_STOCK_ITEM_SKU_EN = "com.gdn.pbp.send.minimum.stock.mail.en";
  String PRODUCT_MAIL_EVENT_SUSPENDED_PRODUCTS = "com.gdn.pbp.send.product.mail.suspended";
  String PRODUCT_MAIL_EVENT_SUSPENDED_PRODUCTS_EN = "com.gdn.pbp.send.product.mail.suspended.en";
  String PRODUCT_MAIL_EVENT_RE_ACTIVATED_PRODUCTS = "com.gdn.pbp.send.product.mail.reactivated";
  String PRODUCT_MAIL_EVENT_RE_ACTIVATED_PRODUCTS_EN = "com.gdn.pbp.send.product.mail.reactivated.en";
  String CREATE_PRODUCT_SYNC_EVENT = "com.gdn.pbp.create.fbb.product.sync.event";
  String IMAGE_RESIZE_EVENT = "com.gdn.pbp.resize.image";
  String IMAGE_RESIZE_PRIORITY_SELLER_EVENT_1 ="com.gdn.pbp.resize.image.priority.1";
  String IMAGE_RESIZE_PRIORITY_SELLER_EVENT_2 ="com.gdn.pbp.resize.image.priority.2";
  String POST_LIVE_REVIEW_PRODUCT_APPROVED_EN = "com.gdn.pbp.send.post.live.product.approved.mail.en";
  String POST_LIVE_REVIEW_PRODUCT_APPROVED = "com.gdn.pbp.send.post.live.product.approved.mail";
  String POST_LIVE_REVIEW_PRODUCT_REJECTED_EN = "com.gdn.pbp.send.post.live.product.rejected.mail.en";
  String POST_LIVE_REVIEW_PRODUCT_REJECTED = "com.gdn.pbp.send.post.live.product.rejected.mail";
  String PRODUCT_STATUS_MAIL_EVENT = "com.gdn.pbp.send.product.status.event";
  String IMAGE_QC_PREDICTION_REQUEST = "com.gdn.image.qc.prediction.request";
  String IMAGE_QC_PREDICTION_RESPONSE = "com.gdn.image.qc.prediction.response";
  String IMAGE_QC_PREDICTION_BACKLOG_REQUEST = "com.gdn.image.qc.backlog.prediction.request";
  String OFFLINE_INVENTORY_STOCK_UPDATED_EVENT_NAME =
      "com.gdn.x.inventory.stock.offline.updated.history.event";
  String PRODUCT_OFFLINE_ITEM_CHANGE = "com.gdn.x.product.offlineitem.change";
  String ITEM_STATUS_EVENT = "com.gdn.pbp.send.item.status.event";
  String MIGRATE_PRODUCTS_EVENT = "com.gdn.pbp.migrate.product.event";
  String EDITED_IMAGE_RESIZE_EVENT = "com.gdn.pbp.edited.resize.image";
  String REVISE_IMAGE_RESIZE_EVENT = "com.gdn.pbp.revised.resize.image";
  String PRODUCT_QC_RETRY_EVENT = "com.gdn.pbp.add.qc.retry.event";
  String PRODUCT_AUTO_NEED_REVISION_EVENT = "com.gdn.pbp.product.auto.need.revision";
  String ADD_PRODUCT_TO_PDT_RETRY = "com.gdn.pbp.product.pdt.retry";
  String RESIGN_SELLER_EVENT = "com.gdn.pbp.resign.seller";
  String PRODUCT_AUTO_APPROVAL_CHECK = "com.gdn.pbp.product.auto.approval.check";
  String PRODUCT_DATA_AUTO_FIX_HISTORY = "com.gdn.pbp.product.data.auto.fix.history";
  String PRODUCT_INTERNAL_HISTORY_SAVE = "com.gdn.pbp.internal.product.history.save";


  String ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT = "com.gdn.pbp.add.product.vendor.combined.event";
  String ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_1 = "com.gdn.pbp.add.product.vendor.combined.event.priority.1";
  String ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT_PRIORITY_2 = "com.gdn.pbp.add.product.vendor.combined.event.priority.2";

  String ACTIVE_PRODUCT_BACK_FILL_FBB_FLAG = "com.gdn.pbp.active.product.back.fill.fbb.flag";

  String INACTIVE_PRODUCT_BACK_FILL_FBB_FLAG = "com.gdn.pbp.inactive.product.back.fill.fbb.flag";

  String FBB_PICKUP_POINT_MIGRATION = "com.gdn.pbp.fbb.pickup.point.migration";
}
