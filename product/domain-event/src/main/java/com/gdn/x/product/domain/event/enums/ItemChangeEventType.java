package com.gdn.x.product.domain.event.enums;

/**
 * Created by govind on 13/09/2018 AD.
 */
public enum ItemChangeEventType {

  ITEM_PRICE_CHANGE("itemPriceChange"),
  ITEM_DATA_CHANGE("itemDataChange"),
  PRISTINE_MAPPING_CHANGE("pristineMappingChange"),
  SHIPPING_CHANGE("shippingChange"),
  OFFLINE_ITEM_FLAG_CHANGE("offlineItemFlagChange"),
  SYNC_UNSYNC_FLAG_CHANGE("sync/unsync flag change"),
  ARCHIVED_FLAG_CHANGE("archiveFlagChange"),
  SUBSCRIPTION_FLAG_CHANGE("subscriptionFlagChange"),
  ITEM_DELETED("itemDeleted"),
  HALAL_CONFIG_CHANGE("halalConfigChange"),
  MASTER_SKU_UPDATE("masterSkuUpdate");

  String name;

  ItemChangeEventType(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }
}
