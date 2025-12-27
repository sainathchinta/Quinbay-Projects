package com.gdn.x.product.model.entity;

import java.util.Date;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class ItemDiscoverableSchedule implements GdnBaseEmbedded {
  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.IS_DISCOVERABLE)
  private boolean isDiscoverable;

  @Field(value = ProductFieldNames.START_DATE_TIME)
  private Date startDateTime;

  @Field(value = ProductFieldNames.END_DATE_TIME)
  private Date endDateTime;

  public ItemDiscoverableSchedule() {

  }

  public ItemDiscoverableSchedule(boolean isDiscoverable, Date startDateTime, Date endDateTime) {
    super();
    this.isDiscoverable = isDiscoverable;
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

  public boolean isDiscoverable() {
    return this.isDiscoverable;
  }

  public void setDiscoverable(boolean isDiscoverable) {
    this.isDiscoverable = isDiscoverable;
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
        "ItemDiscoverableSchedule [isDiscoverable=%s, startDateTime=%s, endDateTime=%s, toString()=%s]",
        this.isDiscoverable, this.startDateTime, this.endDateTime, super.toString());
  }
}
