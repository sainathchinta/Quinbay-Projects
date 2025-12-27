package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemBuyableSchedule implements Serializable {

  private static final long serialVersionUID = 241609259142598211L;

  private boolean isBuyable;

  private Date startDateTime;

  private Date endDateTime;

  public ItemBuyableSchedule() {

  }

  public ItemBuyableSchedule(boolean isDisplayable, Date startDateTime, Date endDateTime) {
    super();
    this.isBuyable = isDisplayable;
    this.startDateTime = startDateTime;
    this.endDateTime = endDateTime;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
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
    return this.isBuyable;
  }

  public void setBuyable(boolean isBuyable) {
    this.isBuyable = isBuyable;
  }

  public void setEndDateTime(Date endDateTime) {
    this.endDateTime = endDateTime;
  }

  public void setStartDateTime(Date startDateTime) {
    this.startDateTime = startDateTime;
  }

  @Override
  public String toString() {
    return String.format(
        "ItemBuyableSchedule [isBuyable=%s, startDateTime=%s, endDateTime=%s, toString()=%s]",
        this.isBuyable, this.startDateTime, this.endDateTime, super.toString());
  }
}
