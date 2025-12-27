package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemViewConfig implements Serializable {

  private static final long serialVersionUID = -2444083325505796216L;

  public static final String COLLECTION_NAME = "prd_product_view_config";

  private boolean isBuyable;

  private boolean isDiscoverable;

  private String channel;

  private ItemDiscoverableSchedule itemDiscoverableSchedules;

  private ItemBuyableSchedule itemBuyableSchedules;

  public ItemViewConfig() {

  }

  public ItemViewConfig(boolean isBuyable, boolean isDiscoverable, String channel,
      ItemDiscoverableSchedule itemDiscoverableSchedules,
      ItemBuyableSchedule itemBuyableSchedules) {
    this.isBuyable = isBuyable;
    this.isDiscoverable = isDiscoverable;
    this.channel = channel;
    this.itemDiscoverableSchedules = itemDiscoverableSchedules;
    this.itemBuyableSchedules = itemBuyableSchedules;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (this.getClass() != obj.getClass())
      return false;
    ItemViewConfig other = (ItemViewConfig) obj;
    if (this.channel == null) {
      if (other.channel != null)
        return false;
    } else if (!this.channel.equals(other.channel))
      return false;
    return true;
  }

  public String getChannel() {
    return this.channel;
  }

  public ItemBuyableSchedule getItemBuyableSchedules() {
    return this.itemBuyableSchedules;
  }

  public ItemDiscoverableSchedule getItemDiscoverableSchedules() {
    return this.itemDiscoverableSchedules;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this.channel == null) ? 0 : this.channel.hashCode());
    return result;
  }

  public boolean isBuyable() {
    return this.isBuyable;
  }

  public boolean isDiscoverable() {
    return this.isDiscoverable;
  }

  public void setBuyable(boolean isBuyable) {
    this.isBuyable = isBuyable;
  }

  public void setChannel(String channel) {
    this.channel = channel;
  }

  public void setDiscoverable(boolean isDiscoverable) {
    this.isDiscoverable = isDiscoverable;
  }

  public void setItemBuyableSchedules(ItemBuyableSchedule itemBuyableSchedules) {
    this.itemBuyableSchedules = itemBuyableSchedules;
  }

  public void setItemDiscoverableSchedules(ItemDiscoverableSchedule itemDiscoverableSchedules) {
    this.itemDiscoverableSchedules = itemDiscoverableSchedules;
  }

  @Override
  public String toString() {
    return String.format(
        "ItemViewConfig [isBuyable=%s, isDiscoverable=%s, channel=%s, itemDiscoverableSchedules=%s, itemBuyableSchedules=%s, toString()=%s]",
        this.isBuyable, this.isDiscoverable, this.channel, this.itemDiscoverableSchedules,
        this.itemBuyableSchedules, super.toString());
  }
}
