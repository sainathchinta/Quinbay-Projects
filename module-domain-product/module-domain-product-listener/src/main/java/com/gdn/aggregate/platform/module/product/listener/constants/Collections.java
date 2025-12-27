package com.gdn.aggregate.platform.module.product.listener.constants;

public interface Collections {

    /*Raw*/
    String UPDATE_QUEUE = "product_update_queue";
    String PRODUCT = "product_product";
    String ITEM = "product_item";
    String PICKUP_POINT = "product_pickup_point";
    String PICKUP_POINT_INVENTORY = "product_pickup_point_inventory";
    String MERCHANT_DISCOUNT_PRICE = "product_merchant_discount_price";
    String ADJUSTMENT_PRODUCT = "product_adjustment_product";
    String ADJUSTMENT_PRODUCT_QUOTA = "product_adjustment_product_quotas";
    String CAMPAIGN_PRODUCT = "product_campaign_products";
    String CHEAPEST_PRICE_DAY = "product_cheapest_price_day";
    String FLASHSALE_PRODUCT = "product_flashsale_products";
    String MASTER_DATA = "product_master_data";
    String MASTER_DATA_PRODUCT = "product_master_data_product";
    String MASTER_DATA_ITEM = "product_master_data_item";

    /*Processed*/
    String SIVA_PRODUCT = "product_siva_product";
    String SIVA_ITEM = "product_siva_item";
    String SIVA_FLASHSALE_SCHEDULE = "product_siva_flashsale_schedules";
    String SIVA_FLASHSALE_GROUP = "product_siva_flashsale_groups";
    String SIVA_CAMPAIGN_PRODUCT = "product_siva_campaign_products";

    /*Other Modules*/
    String INVENTORY_INFORMATION_COLLECTION = "inventory_inventory_infos";
    String INVENTORY_PICKUP_POINT_INFORMATION_COLLECTION = "inventory_inventory_pickup_point_infos";
    String PCB_CATEGORIES = "pcb_categories";
    String MERCHANT = "pdp_merchants";
    String PRODUCT_REVIEW = "product_item_reviews";

    /*Business Partner*/
    String BUSINESS_PARTNER_PICKUP_POINT = "business_partner_pickup_points";
}
