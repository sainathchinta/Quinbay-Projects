package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.Date;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemBuyableScheduleDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private boolean isBuyable;
  private Date startDateTime;
  private Date endDateTime;

  public ItemBuyableScheduleDTO() {

  }

  public ItemBuyableScheduleDTO(boolean isBuyable, Date startDateTime, Date endDateTime) {
    super();
    this.isBuyable = isBuyable;
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
        "ItemBuyableScheduleDTO [isBuyable=%s, startDateTime=%s, endDateTime=%s, toString()=%s]",
        this.isBuyable, this.startDateTime, this.endDateTime, super.toString());
  }


}
