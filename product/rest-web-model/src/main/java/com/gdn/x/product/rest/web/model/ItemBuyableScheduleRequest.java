package com.gdn.x.product.rest.web.model;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemBuyableScheduleRequest extends BaseRequest {

  private static final long serialVersionUID = 1841795694747416692L;

  private String channel;
  private boolean buyable;
  private Date startDateTime;
  private Date endDateTime;

  public ItemBuyableScheduleRequest() {

  }

  public ItemBuyableScheduleRequest(String channel, boolean buyable, Date startDateTime,
      Date endDateTime) {
    this.channel = channel;
    this.buyable = buyable;
    this.startDateTime = startDateTime;
    this.endDateTime = endDateTime;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getChannel() {
    return this.channel;
  }

  public Date getEndDateTime() {
    return this.endDateTime;
  }

  public Date getStartDateTime() {
    return this.startDateTime;
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

  public void setEndDateTime(Date endDateTime) {
    this.endDateTime = endDateTime;
  }

  public void setStartDateTime(Date startDateTime) {
    this.startDateTime = startDateTime;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ItemBuyableScheduleDTO [channel=%s, buyable=%s, startDateTime=%s, endDateTime=%s, toString()=%s]",
            this.channel, this.buyable, this.startDateTime, this.endDateTime, super.toString());
  }

}
