package com.gdn.aggregate.platform.module.product.listener.model.util;

import org.apache.commons.collections.CollectionUtils;

import java.util.Objects;

public enum StockEventType {
  STOCK_UPDATE_SEARCH, STOCK_REPUBLISH, INVENTORY_MODULE_FETCH, CHEAPEST_FALLBACK;

  public static StockEventType from(CompleteItemData data) {
    if (Objects.isNull(data)) return CHEAPEST_FALLBACK;

    return Objects.nonNull(data.getStockUpdateSearchEvent()) ? STOCK_UPDATE_SEARCH
      : Objects.nonNull(data.getLevel2InventoryQuantityChangedEvent()) ? STOCK_REPUBLISH
      : CollectionUtils.isNotEmpty(data.getAllCustomInventoryPickupPointInfos()) ? INVENTORY_MODULE_FETCH
      : CHEAPEST_FALLBACK;
  }

}
