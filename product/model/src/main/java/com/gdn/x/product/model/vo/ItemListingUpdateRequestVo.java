package com.gdn.x.product.model.vo;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.apache.commons.lang3.StringUtils;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ItemListingUpdateRequestVo {
  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;
  private Set<Price> price = new HashSet<>();
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
  private Boolean off2OnChannelActive;
  private Boolean wholesalePriceActivated;

  public ItemViewConfig getSingleItemViewConfigByChannel(String channel) {
    return Optional.ofNullable(itemViewConfigs).orElse(new HashSet<>()).stream()
        .filter(viewConfig -> StringUtils.equals(channel, viewConfig.getChannel()))
        .findFirst().orElse(null);
  }
}
