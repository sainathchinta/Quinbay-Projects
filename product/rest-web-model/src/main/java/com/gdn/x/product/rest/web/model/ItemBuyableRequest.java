package com.gdn.x.product.rest.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemBuyableRequest extends BaseRequest {

  private static final long serialVersionUID = 135787486621807378L;

  private String channel;

  private boolean buyable;

  public ItemBuyableRequest() {}

  public ItemBuyableRequest(String channel, boolean buyable) {
    this.channel = channel;
    this.buyable = buyable;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getChannel() {
    return this.channel;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isBuyable() {
    return this.buyable;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  public void setChannel(String channel) {
    this.channel = channel;
  }

  @Override
  public String toString() {
    return String.format("ItemDiscoverableRequest [channel=%s, buyable=%s, toString()=%s]",
        this.channel, this.buyable, super.toString());
  }
}
