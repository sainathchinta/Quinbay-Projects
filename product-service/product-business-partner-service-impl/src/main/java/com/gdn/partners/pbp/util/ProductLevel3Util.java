package com.gdn.partners.pbp.util;

import java.util.Collection;

import com.gdn.x.product.domain.event.model.ItemViewConfig;
import com.gdn.x.product.domain.event.model.Price;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;

public final class ProductLevel3Util {

  private ProductLevel3Util() {}

  public static ItemViewConfigDTO getItemViewConfigDTO(
      Collection<ItemViewConfigDTO> itemViewConfigs, ChannelName channel) {
    ItemViewConfigDTO result = null;
    for (ItemViewConfigDTO itemViewConfig : itemViewConfigs) {
      if (channel.name().equals(itemViewConfig.getChannel())) {
        result = itemViewConfig;
        break;
      }
    }
    return result;
  }

  public static ItemViewConfig getItemViewConfig(Collection<ItemViewConfig> itemViewConfigs,
      ChannelName channel) {
    ItemViewConfig result = null;
    for (ItemViewConfig itemViewConfig : itemViewConfigs) {
      if (channel.name().equals(itemViewConfig.getChannel())) {
        result = itemViewConfig;
        break;
      }
    }
    return result;
  }

  public static Price getItemPrice(Collection<Price> itemPrices, ChannelName channel) {
    Price result = null;
    for (Price itemPrice : itemPrices) {
      if (channel.name().equals(itemPrice.getChannel())) {
        result = itemPrice;
        break;
      }
    }
    return result;
  }
}
