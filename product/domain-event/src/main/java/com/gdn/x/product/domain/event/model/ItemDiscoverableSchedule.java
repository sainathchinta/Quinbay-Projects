package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemDiscoverableSchedule implements Serializable {

  private static final long serialVersionUID = -941698729592665796L;

  private boolean isDiscoverable;

  private Date startDateTime;

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
    return String
        .format(
            "ItemDiscoverableSchedule [isDiscoverable=%s, startDateTime=%s, endDateTime=%s, toString()=%s]",
            this.isDiscoverable, this.startDateTime, this.endDateTime, super.toString());
  }
}
