package com.gdn.x.product.rest.web.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemViewConfigRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private boolean isBuyable;
  private boolean isDiscoverable;
  private String channel = ChannelName.DEFAULT.toString();
  private ItemDiscoverableScheduleDTO itemDiscoverableSchedules;
  private ItemBuyableScheduleDTO itemBuyableSchedules;

  public ItemViewConfigRequest() {

  }

  public ItemViewConfigRequest(boolean isBuyable, boolean isDiscoverable, String channel,
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
    return String
        .format(
            "ItemViewConfigRequest [isBuyable=%s, isDiscoverable=%s, channel=%s, itemDiscoverableSchedules=%s, itemBuyableSchedules=%s, toString()=%s]",
            this.isBuyable, this.isDiscoverable, this.channel, this.itemDiscoverableSchedules,
            this.itemBuyableSchedules, super.toString());
  }

}
