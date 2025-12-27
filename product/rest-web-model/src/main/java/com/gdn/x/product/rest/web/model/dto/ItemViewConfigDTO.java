package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import lombok.Data;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ChannelName;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemViewConfigDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private boolean isBuyable;
  private boolean isDiscoverable;
  private String channel = ChannelName.DEFAULT.toString();
  private ItemDiscoverableScheduleDTO itemDiscoverableSchedules;
  private ItemBuyableScheduleDTO itemBuyableSchedules;
  private boolean isBuyableOriginal;
  private boolean isDiscoverableOriginal;

  public ItemViewConfigDTO() {

  }

  public ItemViewConfigDTO(boolean isBuyable, boolean isDiscoverable, String channel,
      ItemDiscoverableScheduleDTO itemDiscoverableSchedules,
      ItemBuyableScheduleDTO itemBuyableSchedules) {
    super();
    this.isBuyable = isBuyable;
    this.isDiscoverable = isDiscoverable;
    this.channel = channel;
    this.itemDiscoverableSchedules = itemDiscoverableSchedules;
    this.itemBuyableSchedules = itemBuyableSchedules;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }


  public String getChannel() {
    return this.channel;
  }

  public ItemBuyableScheduleDTO getItemBuyableSchedules() {
    return this.itemBuyableSchedules;
  }

  public ItemDiscoverableScheduleDTO getItemDiscoverableSchedules() {
    return this.itemDiscoverableSchedules;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
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

  public void setItemBuyableSchedules(ItemBuyableScheduleDTO itemBuyableSchedules) {
    this.itemBuyableSchedules = itemBuyableSchedules;
  }

  public void setItemDiscoverableSchedules(ItemDiscoverableScheduleDTO itemDiscoverableSchedules) {
    this.itemDiscoverableSchedules = itemDiscoverableSchedules;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ItemViewConfigDTO [isBuyable=").append(isBuyable).append(", isDiscoverable=")
        .append(isDiscoverable).append(", channel=").append(channel)
        .append(", itemDiscoverableSchedules=").append(itemDiscoverableSchedules)
        .append(", itemBuyableSchedules=").append(itemBuyableSchedules).append("]");
    return builder.toString();
  }

}
