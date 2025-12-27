package com.gdn.x.product.model.vo;

import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.apache.commons.lang3.StringUtils;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
public class ItemPickupPointListingUpdateRequestVo {
  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;
  private Set<Price> price = new HashSet<>();
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
  private boolean off2OnChannelActive;
  private Boolean wholesalePriceActivated;
  private boolean cncActivated;
  private boolean fbbActivated;
  private Boolean distribution;
  private String productSku;
  private String merchantCode;
  private boolean ppCodeChangedForNonMppSeller;
  private B2bFieldsVo b2bFieldsVo;
  private boolean scheduleRemoval; // will be true if L5 status is changed
  private BuyableScheduleVo buyableScheduleVo;
  private DiscoverableScheduleVo discoverableScheduleVo;

  public ItemViewConfig getSingleItemViewConfigByChannel(String channel) {
    return Optional.ofNullable(itemViewConfigs).orElse(new HashSet<>()).stream()
        .filter(viewConfig -> StringUtils.equals(channel, viewConfig.getChannel()))
        .findFirst().orElse(null);
  }
}
