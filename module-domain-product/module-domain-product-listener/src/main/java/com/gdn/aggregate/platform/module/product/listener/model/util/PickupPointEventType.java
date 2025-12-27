package com.gdn.aggregate.platform.module.product.listener.model.util;

import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointUpsertCombinedEventModel;
import java.util.Objects;

public enum PickupPointEventType {
  PICKUP_POINT,
  INVENTORY_INFO_CHANGE,
  STOCK_UPDATE_SEARCH,
  INVENTORY_REPUBLISH,
  UNKNOWN;

  /**
   * Determines the PickupPointEventType from the given event model.
   */
  public static PickupPointEventType from(PickupPointUpsertCombinedEventModel eventModel) {
    if (Objects.isNull(eventModel)) return UNKNOWN;
    return Objects.nonNull(eventModel.getPickupPoint()) ? PICKUP_POINT
      : Objects.nonNull(eventModel.getInventoryInfoChange()) ? INVENTORY_INFO_CHANGE
      : Objects.nonNull(eventModel.getStockUpdateSearchEvent()) ? STOCK_UPDATE_SEARCH
      : Objects.nonNull(eventModel.getLevel2InventoryQuantityChangedEvent()) ? INVENTORY_REPUBLISH
      : UNKNOWN;
  }

}
