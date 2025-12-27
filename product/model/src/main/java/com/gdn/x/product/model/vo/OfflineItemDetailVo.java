package com.gdn.x.product.model.vo;

import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;

import java.util.Set;

/**
 * Created by william.s.setiadi on 4/16/2018.
 */
public class OfflineItemDetailVo {

  private String uniqueId;
  private String itemSku;
  private String pickupPointCode;
  private Set<Price> prices;
  private Set<ItemViewConfig> itemViewConfigs;

  public OfflineItemDetailVo() {
  }

  public OfflineItemDetailVo(String uniqueId, String itemSku, String pickupPointCode,
      Set<Price> prices, Set<ItemViewConfig> itemViewConfigs) {
    this.uniqueId = uniqueId;
    this.itemSku = itemSku;
    this.pickupPointCode = pickupPointCode;
    this.prices = prices;
    this.itemViewConfigs = itemViewConfigs;
  }

  public String getUniqueId() {
    return uniqueId;
  }

  public void setUniqueId(String uniqueId) {
    this.uniqueId = uniqueId;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Set<Price> getPrices() {
    return prices;
  }

  public void setPrices(Set<Price> prices) {
    this.prices = prices;
  }

  public Set<ItemViewConfig> getItemViewConfigs() {
    return itemViewConfigs;
  }

  public void setItemViewConfigs(Set<ItemViewConfig> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("OfflineItemDetailVo{");
    sb.append("uniqueId='").append(uniqueId).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", prices=").append(prices);
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append('}');
    return sb.toString();
  }
}
