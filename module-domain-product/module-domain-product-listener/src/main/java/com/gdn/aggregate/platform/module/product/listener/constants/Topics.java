package com.gdn.aggregate.platform.module.product.listener.constants;

public interface Topics {

    /*Scheduler Platform*/
    String SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_SCHEDULE = "com.gdn.module.product.siva.flashsale.schedule.delete.all.expired";
    String SCHEDULE_NAME_DELETE_SIVA_FLASHSALE_SCHEDULE = "schedule-delete-siva-flashsale-schedule";

    String SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_GROUP = "com.gdn.module.product.siva.flashsale.group.delete.all.expired";
    String SCHEDULE_NAME_DELETE_SIVA_FLASHSALE_GROUP = "schedule-delete-siva-flashsale-group";

    String SCHEDULE_DELETE_ALL_EXPIRED_SIVA_CAMPAIGN_PRODUCT = "com.gdn.module.product.siva.campaign.product.delete.all.expired";
    String SCHEDULE_NAME_DELETE_SIVA_CAMPAIGN_PRODUCT = "schedule-delete-siva-campaign-product";

    String UPDATE_QUEUE_DENPASAR = "com.gdn.cms.backend.update.queue";
    String UPDATE_QUEUE_MODULE_PRODUCT = "com.gdn.aggregate.modules.product.update.queue";

    /*Direct Update SivaProduct Flow*/
    String GROUP_ID_SIVA_PRODUCT = "agp_module_product_siva_product";
    String GROUP_ID_SIVA_PRODUCT_HIGH_VOLUME = "agp_module_product_siva_product_high_volume";
    String GROUP_ID_SIVA_PRODUCT_BY_DIGITAL_FLASHSALE = "agp_module_product_siva_product_by_digital_flashsale";

    /*Direct Update SivaItem Flow*/
    String GROUP_ID_SIVA_ITEM = "agp_module_product_siva_item";
    /*Direct Update Both SivaProduct & SivaItem Flow*/
    String GROUP_ID_SIVA_BOTH = "agp_module_product_siva_both";
    String GROUP_ID_SIVA_BOTH_BY_UPDATE_QUEUE_L3 = "agp_module_product_siva_both_by_update_queue_l3";
    String GROUP_ID_SIVA_BOTH_BY_UPDATE_QUEUE_L4 = "agp_module_product_siva_both_by_update_queue_l4";
    String GROUP_ID_SIVA_BOTH_BY_LOGISTIC_CHANGE = "agp_module_product_siva_both_by_logistic_change";
    String GROUP_ID_SIVA_BOTH_BY_BUSINESS_PARTNER_PROFILE_CHANGE = "agp_module_product_siva_both_by_business_partner_profile_change";
    String GROUP_ID_SIVA_BOTH_BY_FBB_CHANGE = "agp_module_product_siva_both_by_fbb_change";
    String GROUP_ID_SIVA_BOTH_BY_TRADE_IN_CHANGE = "agp_module_product_siva_both_by_trade_in_change";
    String GROUP_ID_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT = "agp_module_product_siva_both_by_adjustment_product";
    String GROUP_ID_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_FLASHSALE = "agp_module_product_siva_both_by_adjustment_product_flashsale";
    String GROUP_ID_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_QUOTA = "agp_module_product_siva_both_by_adjustment_product_quota";
    String GROUP_ID_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_QUOTA_FLASHSALE = "agp_module_product_siva_both_by_adjustment_product_quota_flashsale";
    String GROUP_ID_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_SAVED = "agp_module_product_siva_both_by_adjustment_product_saved";
    String GROUP_ID_SIVA_BOTH_BY_ADJUSTMENT_PRODUCT_SAVED_FLASHSALE = "agp_module_product_siva_both_by_adjustment_product_saved_flashsale";
    String GROUP_ID_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE = "agp_module_product_siva_both_by_inventory_info_change";
    String GROUP_ID_SIVA_BOTH_BY_INVENTORY_STATUS_CHANGE = "agp_module_product_siva_both_by_inventory_status_change";
    String GROUP_ID_SIVA_BOTH_BY_MERCHANT_DISCOUNT_PRICE_CHANGE = "agp_module_product_siva_both_by_merchant_discount_price_change";
    String GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT = "agp_module_product_siva_both_by_pickup_point";
    String GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT_HIGH_VOLUME = "agp_module_product_siva_both_by_pickup_point_high_volume";
    String GROUP_ID_SIVA_BOTH_BY_ITEM_HIGH_VOLUME = "agp_module_product_siva_both_by_item_high_volume";
    String GROUP_ID_SIVA_BOTH_BY_STORE_CLOSING_MERCHANT_CHANGE = "agp_module_product_siva_both_by_store_closing_merchant_change";
    String GROUP_ID_SIVA_BOTH_BY_PRODUCT_REVIEW = "agp_module_product_siva_both_by_product_review";
    String GROUP_ID_SIVA_BOTH_BY_FLAHSALE_PRODUCT = "agp_module_product_siva_both_by_flashsale_product";
    String GROUP_ID_SIVA_BOTH_BY_CHEAPEST_PRICE = "agp_module_product_siva_both_by_cheapest_product";
    String GROUP_ID_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_PUBLISHED = "agp_module_product_siva_both_by_campaign_product_published";
    String GROUP_ID_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE = "agp_module_product_siva_both_by_campaign_product_tag_label_change";
    String GROUP_ID_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_ENDED = "agp_module_product_siva_both_by_campaign_product_ended";
    String GROUP_ID_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_REMOVED = "agp_module_product_siva_both_by_campaign_product_removed";
    String GROUP_ID_SIVA_BOTH_BY_MASTER_DATA = "agp_module_product_siva_both_by_master_data";
    String GROUP_ID_SIVA_BOTH_BY_PRODUCT = "agp_module_product_siva_both_by_product";
    String GROUP_ID_SIVA_BOTH_BY_ITEM = "agp_module_product_siva_both_by_item";
    String GROUP_ID_SIVA_BOTH_BY_FBB_ITEM = "agp_module_product_siva_both_by_fbb_item";

    /*Direct Update SivaFlashsaleSchedule Flow*/
    String GROUP_ID_SIVA_FLASHSALE_SCHEDULE = "agp_module_product_siva_flashsale_schedule";
    String GROUP_ID_SIVA_FLASHSALE_SCHEDULE_BY_CLEAN = "agp_module_product_siva_flashsale_schedule_by_clean";
    String GROUP_ID_SIVA_FLASHSALE_GROUP_BY_CLEAN = "agp_module_product_siva_flashsale_group_by_clean";
    String GROUP_ID_SIVA_FLASHSALE_SCHEDULE_BY_DEACTIVATE = "agp_module_product_siva_flashsale_schedule_by_deactivate";
    String GROUP_ID_SIVA_FLASHSALE_GROUP_BY_DEACTIVATE = "agp_module_product_siva_flashsale_group_by_deactivate";
    String GROUP_ID_SIVA_FLASHSALE_SCHEDULE_BY_ALL_EXPIRED = "agp_module_product_siva_flashsale_schedule_by_all_expired";

    /*Direct Update SivaFlashsaleGroup Flow*/
    String GROUP_ID_SIVA_FLASHSALE_GROUP = "agp_module_product_siva_flashsale_group";
    String GROUP_ID_SIVA_FLASHSALE_GROUP_BY_ALL_EXPIRED = "agp_module_product_siva_flashsale_group_by_all_expired";

    /*Direct Update SivaCampaignProduct Flow*/
    String GROUP_ID_SIVA_CAMPAIGN_PRODUCT = "agp_module_product_siva_campaign_product";
    String GROUP_ID_SIVA_CAMPAIGN_PRODUCT_BY_CAMPAIGN_PRODUCT_ENDED = "agp_module_product_siva_campaign_product_by_campaign_product_ended";
    String GROUP_ID_SIVA_CAMPAIGN_PRODUCT_BY_CAMPAIGN_TEASER_LIVE = "agp_module_product_siva_campaign_product_by_campaign_teaser_live";
    String GROUP_ID_SIVA_CAMPAIGN_PRODUCT_BY_ALL_EXPIRED = "agp_module_product_siva_campaign_product_by_all_expired";

    /*Trigger Construct by productSku and itemSku from Denpasar*/
    String GROUP_ID_FULL_RECONSTRUCT_BY_PRODUCT_SKU = "agp_module_product_full_reconstruct_by_product_sku";
    String GROUP_ID_FULL_RECONSTRUCT_BY_ITEM_SKU = "agp_module_product_full_reconstruct_by_item_sku";
    String GROUP_ID_PERMANENT_DELETE_DATA_FROM_AGP = "agp_module_product_permanent_delete_data_from_agp";

    /*Changes*/
    String PRODUCT = "com.gdn.x.product.product.change";
    String ITEM = "com.gdn.x.product.item.data.change";
    String NEW_RAW_ITEM_COLLECTION_UPDATE_EVENT = "com.gdn.agp.raw.item.data.update.queue";
    String NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT = "com.gdn.agp.siva.item.data.update.queue";
    String NEW_SIVA_ITEM_COLLECTION_UPDATE_EVENT_HIGH_VOLUME = "com.gdn.agp.siva.item.data.update.queue.high.volume";
    String NEW_RAW_ITEM_COLLECTION_UPDATE_EVENT_HIGH_VOLUME = "com.gdn.agp.raw.item.data.update.queue.high.volume";
    String PICKUP_POINT = "com.gdn.x.product.item.pickup.point.data.change";
    String NEW_PICKUP_POINT_UPSERT_EVENT = "com.gdn.agp.pickup.point.data.update.queue";
    String NEW_PICKUP_POINT_UPSERT_EVENT_HIGH_VOLUME = "com.gdn.agp.pickup.point.data.update.queue.high.volume";
    String ADJUSTMENT_PRODUCT = "com.gdn.x.promotion.adjustment.product.change.v2";
    String ADJUSTMENT_PRODUCT_QUOTA = "com.gdn.x.promotion.adjustment.product.quota.updated";
    String ADJUSTMENT_PRODUCT_SAVED = "com.gdn.x.promotion.adjustment.product.saved";
    String MASTER_DATA = "com.gdn.x.productcategorybase.product.publish";
    String MERCHANT_DISCOUNT_PRICE = "com.gdn.x.product.merchant.promo.discount.price.change";
    String INVENTORY_INFO_CHANGE = "com.gdn.aggregate.modules.inventory.inventory.info.change";
    String INVENTORY_NON_OOS = "com.gdn.x.inventory.stock.non.oos.event";
    String INVENTORY_OOS = "com.gdn.x.inventory.stock.oos.event";
    String PRODUCT_REVIEW_CHANGE = "com.gdn.aggregate.modules.product.item.product.review.change";
    String STORE_CLOSING_MERCHANT_CHANGE = "com.gdn.aggregate.modules.pdp.store.closing.merchant.change";
    String BUSINESS_PARTNER_PROFILE_CHANGE = "com.gdn.aggregate.modules.pdp.business.partner.profile.change";
    String FBB_CHANGE = "com.gdn.aggregate.modules.product.fbb.change";
    String TRADE_IN_CHANGE = "com.gdn.trade.in.whitelisted.configuration";
    String DENPASAR_SHIPPING_LOGISTIC_OPTION_CHANGE = "com.gdn.denpasar.shipping.logistic.option.change.event";
    String DENPASAR_SEARCH_LOGISTIC_OPTION_CHANGE = "com.gdn.denpasar.search.logistic.option.change.event";
    String CAMPAIGN_PRODUCT_PUBLISHED = "com.gdn.x.campaign.published";
    String CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE = "com.gdn.aggregate.modules.product.tag.label.change";
    String CHEAPEST_PRICE = "com.partners.promo.analytics.campaign.cheapest.price.days";
    String FBB_ITEM_SAVE = "fbb.core.product.item.save";
    String DIGITAL_FLASHSALE = "com.gdn.x.pulsa.digital.flashsale";

    /*Migrations*/
    String ALL_PRODUCT = "com.gdn.x.product.product.all";
    String ALL_ITEM = "com.gdn.x.product.item.data.change.agp";
    String ALL_PICKUP_POINT = "com.gdn.x.product.item.pickup.point.data.change.agp";
    String ALL_ADJUSTMENT_PRODUCT = "com.gdn.x.promotion.adjustment.product.change.v2.agp";
    String ALL_MASTER_DATA = "com.gdn.x.productcategorybase.product.publish.all";
    String ALL_DIGITAL_FLASHSALE = "com.gdn.x.pulsa.digital.flashsale.republish";

    /*Processed*/
    String ID_TIMESTAMP_SIVA_PRODUCT = "com.gdn.aggregate.modules.domain.product.id.timestamp.siva.product";
    String ID_TIMESTAMP_SIVA_ITEM = "com.gdn.aggregate.modules.domain.product.id.timestamp.siva.item";
    String FIX_PRICE = "com.gdn.aggregate.modules.product.fix.price";
    String FIX_ITEM_CODE = "com.gdn.aggregate.modules.product.fix.item.code";
    String LKPP_PRODUCT_ID = "com.gdn.b2g.lkpp.product.id";
    String PRODUCTFEED_ITEM_ID = "com.gdn.x.productfeed.item.id";
    String PRODUCTFEED_ITEM_ID_REPUBLISH = "com.gdn.x.productfeed.item.id.republish";

    /*Semi*/
    String FLASHSALE_PRODUCT = "com.gdn.pancen.flashsale.product.change";
    String FLASHSALE_GROUP = "com.gdn.pancen.flashsale.group.change";
    String FLASHSALE_SCHEDULE = "com.gdn.pancen.flashsale.schedule.change";
    String CLEAN_FLASHSALE_SCHEDULE = "com.gdn.pancen.flashsale.schedule.clean";
    String CLEAN_FLASHSALE_GROUP = "com.gdn.pancen.flashsale.group.clean";
    String DEACTIVATE_FLASHSALE_SCHEDULE = "com.gdn.aggregate.modules.product.deactivate.flashsale.schedule";
    String DEACTIVATE_FLASHSALE_GROUP = "com.gdn.aggregate.modules.product.deactivate.flashsale.group";
    String CAMPAIGN_PRODUCT_LIVE = "com.gdn.x.campaign.live";
    String CAMPAIGN_PRODUCT_ENDED = "com.gdn.x.campaign.ended";
    String CAMPAIGN_PRODUCT_REMOVED = "com.gdn.x.campaign.product.removed";
    String CAMPAIGN_TEASER_LIVE = "com.gdn.x.campaign.teaser.live";

    /*Construct*/
    String PRODUCT_DENPASAR = "com.gdn.denpasar.product.construct";
    String ITEM_DENPASAR = "com.gdn.denpasar.item.construct";
    String SIVA_PRODUCT_COMBINED_UPSERT = "com.gdn.aggregate.modules.siva.product.combined.upsert";
    String SIVA_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME = "com.gdn.aggregate.modules.siva.product.combined.upsert.high.volume";
    String RAW_PRODUCT_COMBINED_UPSERT = "com.gdn.aggregate.modules.raw.product.combined.upsert";
    String RAW_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME = "com.gdn.aggregate.modules.raw.product.combined.upsert.high.volume";
    String PERMANENT_DELETE_DATA_FROM_AGP ="com.gdn.product.analytics.permanent.delete.product.agp";
    String INVENTORY_STOCK_UPDATE_EVENT = "com.gdn.x.inventory.stock.update.search.event";
    String INVENTORY_ONLINE_REPUBLISH_EVENT = "com.gdn.x.inventory.stock.quantity.republish.event";
    String PRODUCT_ES_DELETION_EVENT = "com.gdn.aggregate.modules.product.elasticsearch.deletion";

}
