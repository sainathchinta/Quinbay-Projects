package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointResponse;

import java.util.Set;

public interface ItemPickupPointHelperService {

  /**
   * checking existing price and new price are same or not
   * @param item
   * @param prices
   * @return
   */
  boolean isItemPickupPointPriceChange(ItemPickupPoint item, Set<Price> prices);

  /**
   * Check if price is updated on original selling price
   *
   * @param itemPickupPoint
   * @param prices
   * @return
   */
  boolean isItemPickupPointPriceChangeForOriginalSellingPrice(ItemPickupPoint itemPickupPoint, Set<Price> prices);

  /**
   * set new item price
   * @param item
   * @param prices
   * @param username
   * @return
   */
  ItemPickupPoint setItemPriceByChannel(ItemPickupPoint item, Set<Price> prices, String username);

  /**
   *
   * @param itemPickupPoint
   * @return
   */
  boolean isPriceEditDisabled(ItemPickupPoint itemPickupPoint);

  /**
   * To check if itemViewConfig is changed
   * @param itemPickupPoint
   * @param itemViewConfigs
   * @return
   */
  boolean isItemViewConfigChangeForExistingChannel(ItemPickupPoint itemPickupPoint,
    Set<ItemViewConfig> itemViewConfigs);

  /**
   * To send response
   *
   * @param autoCreatePickupPointResponse
   * @param itemPickupPoint
   * @return
   */
  AutoCreatePickupPointResponse getAutoCreatePickupPointResponse(
      AutoCreatePickupPointResponse autoCreatePickupPointResponse, ItemPickupPoint itemPickupPoint);

  /**
   * To set itemViewConfig
   *
   * @param itemPickupPoint
   * @param deliveryFlag
   * @return
   */
  void setItemViewConfig(ItemPickupPoint itemPickupPoint, boolean deliveryFlag,
      ItemPickupPoint resultMaxPriceItemPickupPoint);
}
