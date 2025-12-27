package com.gdn.aggregate.platform.module.product.listener.constants;

public interface Enabler {

  String JACKSON_ENABLED = "module.domain.product.jackson.enabled";

  /*Direct Update Both SivaProduct & SivaItem Flow*/
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_UPDATE_QUEUE = "module.domain.product.siva.both.by.update.queue";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_LOGISTIC_OPTION_CHANGE = "module.domain.product.siva.both.by.logistic.option.change";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_FBB_TAG = "module.domain.product.siva.both.by.fbb.tag";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_TRADE_IN_TAG = "module.domain.product.siva.both.by.trade.in.tag";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT = "module.domain.product.siva.both.by.adjustment.product";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_FLASHSALE = "module.domain.product.siva.both.by.adjustment.product.flashsale";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_QUOTA = "module.domain.product.siva.both.by.adjustment.product.quota";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_QUOTA_FLASHSALE = "module.domain.product.siva.both.by.adjustment.product.quota.flashsale";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_SAVED = "module.domain.product.siva.both.by.adjustment.product.saved";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_SAVED_FLASHSALE = "module.domain.product.siva.both.by.adjustment.product.saved.flashsale";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE = "module.domain.product.siva.both.by.inventory.info.change";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_INVENTORY_STATUS_CHANGE = "module.domain.product.siva.both.by.inventory.status.change";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_MERCHANT_DISCOUNT_PRICE_CHANGE = "module.domain.product.siva.both.by.merchant.discount.price.change";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_PICKUP_POINT = "module.domain.product.siva.both.by.pickup.point";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_STORE_CLOSING_MERCHANT_CHANGE = "module.domain.product.siva.both.by.store.closing.merchant.change";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_PRODUCT_REVIEW = "module.domain.product.siva.both.by.product.review";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_FLASHSALE_PRODUCT = "module.domain.product.siva.both.by.flashsale.product";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_CHEAPEST_PRICE = "module.domain.product.siva.both.by.cheapest.price";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_PUBLISHED = "module.domain.product.siva.both.by.campaign.product.published";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_ENDED = "module.domain.product.siva.both.by.campaign.product.ended";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_REMOVED = "module.domain.product.siva.both.by.campaign.product.removed";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_MASTER_DATA = "module.domain.product.siva.both.by.master.data";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_PRODUCT = "module.domain.product.siva.both.by.product";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_ITEM = "module.domain.product.siva.both.by.item";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_FBB_ITEM = "module.domain.product.siva.both.by.fbb.item";

  /*Direct Update SivaProduct Flow*/
  String FLOW_DIRECT_UPDATE_SIVA_PRODUCT_BY_DIGITAL_FLASHALE = "module.domain.product.siva.product.by.digital.flashsale";

  /*Direct Update SivaFlashsaleSchedule Flow*/
  String FLOW_DIRECT_UPDATE_SIVA_FLASHSALE_SCHEDULE = "module.domain.product.siva.flashsale.schedule";

  /*Direct Update SivaFlashsaleGroup Flow*/
  String FLOW_DIRECT_UPDATE_SIVA_FLASHSALE_GROUP = "module.domain.product.siva.flashsale.group";

  /*Direct Update SivaCampaignProduct Flow*/
  String FLOW_DIRECT_UPDATE_SIVA_CAMPAIGN_PRODUCT = "module.domain.product.siva.campaign.product";

  /*Direct Construct from Denpasar*/
  String FLOW_FULL_RECONSTRUCT = "module.domain.product.full.reconstruct";
  String FLOW_SIVA_PRODUCT_COMBINED_UPSERT = "module.domain.product.siva.product.combined.upsert";
  String FLOW_PICKUP_POINT_CHANGE_COMBINED_EVENT = "pickup.point.change.queue.enabled";
  String FLOW_RAW_PRODUCT_COMBINED_UPSERT = "module.domain.product.raw.product.combined.upsert";
  String FLOW_SIVA_ITEM_COMBINED_UPSERT = "new.item.data.change.queue.enabled";
  String FLOW_RAW_ITEM_COMBINED_UPSERT = "new.raw.item.data.change.queue.enabled";
  String FLOW_SIVA_ITEM_COMBINED_UPSERT_ENABLED = "module.domain.product.siva.item.combined.upsert";
  String FLOW_INVENTORY_INFO_COMBINED_UPSERT = "new.inventory.info.change.queue.enabled";
  String FLOW_PERMANENT_DELETE_DATA = "module.domain.product.permanent.delete.data";

  /*Batch Deduplication Listeners*/
  String FLOW_SIVA_ITEM_BATCH_DEDUPLICATION_ENABLED = "module.domain.product.siva.item.batch.deduplication.enabled";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_STOCK_UPDATE =
      "module.domain.product.siva.both.by.stock.update";
  String FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_STOCK_REPUBLISH =
    "module.domain.product.siva.both.by.stock.republish";
  String FLOW_SIVA_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME =
    "module.domain.product.siva.product.combined.upsert.high.volume";
  String FLOW_PICKUP_POINT_CHANGE_COMBINED_EVENT_HIGH_VOLUME =
    "module.domain.product.pickup.point.change.queue.enabled.high.volume";
  String FLOW_SIVA_ITEM_COMBINED_UPSERT_ENABLED_HIGH_VOLUME =
  "module.domain.product.siva.item.combined.upsert.high.volume";
  String FLOW_RAW_ITEM_COMBINED_UPSERT_HIGH_VOLUME =
  "module.domain.product.raw.item.combined.upsert.high.volume";
  String FLOW_RAW_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME =
  "module.domain.product.raw.product.combined.upsert.high.volume";
}
