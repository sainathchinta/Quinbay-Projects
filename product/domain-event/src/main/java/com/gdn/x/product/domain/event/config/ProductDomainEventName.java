package com.gdn.x.product.domain.event.config;

public class ProductDomainEventName {
  public static final String PRODUCT_ALL_EVENT_NAME = "com.gdn.x.product.product.all";
  public static final String PRODUCT_CHANGE_EVENT_NAME = "com.gdn.x.product.product.change";
  public static final String ITEM_ALL_EVENT_NAME = "com.gdn.x.product.item.all";
  public static final String ITEM_CHANGE_EVENT_NAME = "com.gdn.x.product.item.change";
  public static final String ITEM_DATA_CHANGE_EVENT_NAME = "com.gdn.x.product.item.data.change";
  public static final String ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME = "com.gdn.x.product.item.pickup.point.data.change";
  public static final String PRODUCT_PREORDER_STATUS = "com.gdn.x.product.product.preorder.status";
  public static final String ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME_AGP = "com.gdn.x.product.item.pickup.point.data.change.agp";
  public static final String PRISTINE_ITEM_CHANGE_EVENT_NAME = "com.gdn.x.product.pristine.item.change";
  public static final String OFFLINE_ITEM_CHANGE_EVENT_NAME = "com.gdn.x.product.offlineitem.change";
  public static final String OFFLINE_ITEM_CHANGE_REGULAR_SELLER_EVENT_NAME = "com.gdn.x.product.offlineitem.change.regular.seller";
  public static final String OFFLINE_ITEM_CHANGE_BIG_SELLER_EVENT_NAME = "com.gdn.x.product.offlineitem.change.big.seller";
  public static final String POPULATE_OFFLINE_ITEM_EVENT_NAME = "com.gdn.x.product.offlineitem.populate";
  public static final String ITEM_PRICE_CHANGE_EVENT_NAME = "com.gdn.x.product.item.price.change";
  public static final String CATEGORY_TO_PRODUCT_SKU_MAPPING_EVENT = "com.gdn.mta.bulk.category.to.product.sku.mapping";
  public static final String CATEGORY_TO_PRODUCT_SKU_SAVE_EVENT = "com.gdn.mta.save.category.product.sku.mapping";
  public static final String PRODUCT_SKU_TO_SALES_CATALOG_MAPPING_EVENT = "com.gdn.mta.bulk.product.sku.to.sales.catalog.mapping";
  public static final String PRODUCT_SKU_TO_SALES_CATALOG_SAVE_EVENT = "com.gdn.mta.save.product.sku.to.sales.catalog.mapping";
  public static final String MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE = "com.gdn.x.product.merchant.promo.discount.price.change";
  public static final String REINDEX_PRODUCTS_TO_SOLR = "com.gdn.x.product.reindex.products.to.solr";
  public static final String PRODUCT_ITEM_VIEW_CONFIG_CHANGE = "com.gdn.x.product.item.view.config.change";
  public static final String FBB_ITEM_SYNC_EVENT = "com.gdn.fbb.item.sync.event";
  public static final String BLIMART_SUBSCRIPTION_EVENT = "com.gdn.subscription.product.activated";
  public static final String WHOLESALE_PRICE_ACTIVATED_DEACTIVATED_BY_SCHEDULER =
      "com.gdn.wholesale.price.scheduler.activated.change";
  public static final String PRODUCT_L3_SOLR_REINDEX_EVENT_NAME =
      "com.gdn.x.product.product.l3.solr.reindex";
  public static final String L4_SOLR_REINDEX_EVENT_NAME =
      "com.gdn.x.product.l4.solr.reindex";
  public static final String PRODUCT_SKU_UPDATE_HISTORY = "com.gdn.x.product.productsku.update.history";
  public static final String UPDATE_TO_SOLR = "com.gdn.x.product.update.to.solr";
  public static final String ITEM_MIGRATION_EVENT = "com.gdn.x.product.item.migration";
  public static final String MIGRATION_FOR_CREATION_EVENT = "com.gdn.x.product.migration.for.creation";
  public static final String DELETE_REJECTED_MERCHANT_PRODUCT_EVENT =
      "com.gdn.x.productcategorybase.delete.rejected.merchant.product";
  public static final String ACTIVE_PRODUCT_BACK_FILL_FBB_FLAG =
    "com.gdn.pbp.active.product.back.fill.fbb.flag";

  public static final String ITEM_CACHE_CLEAR_EVENT = "com.gdn.x.product.item.cache.clear";

  public static final String HALAL_HISTORY_UPDATE = "com.gdn.x.product.halal.history.update";
  public static final String TRIGGER_REINDEX_WITH_EXTERNAL_SEARCH =
      "com.gdn.x.product.trigger.reindex.with.external.search";

  public static final String PRODUCT_BUNDLE_ONE_TO_ONE_MAPPING_EVENT =
      "com.gdn.x.product.product.bundle.one.to.one.mapping.event";
  public static final String MASTER_SKU_MAPPING_DEMAPPING = "com.gdn.msku.item.mapping.demapping";
  public static final String SEND_EVENT_FOR_DISTRIBUTION_HISTORY = "com.gdn.pbp.product.distribution.history";
}
