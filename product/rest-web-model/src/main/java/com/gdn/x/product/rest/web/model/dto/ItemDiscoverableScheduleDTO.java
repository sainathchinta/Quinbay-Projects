package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.Date;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemDiscoverableScheduleDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private boolean isDiscoverable;
  private Date startDateTime;
  private Date endDateTime;

  public ItemDiscoverableScheduleDTO() {

  }

  public ItemDiscoverableScheduleDTO(boolean isDiscoverable, Date startDateTime, Date endDateTime) {
    this.isDiscoverable = isDiscoverable;
    this.startDateTime = startDateTime;
    this.endDateTime = endDateTime;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
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
            "ItemDiscoverableScheduleDTO [isDiscoverable=%s, startDateTime=%s, endDateTime=%s, toString()=%s]",
            this.isDiscoverable, this.startDateTime, this.endDateTime, super.toString());
  }
}
