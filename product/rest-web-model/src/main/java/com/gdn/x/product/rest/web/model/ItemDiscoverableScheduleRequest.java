package com.gdn.x.product.rest.web.model;

import java.util.Date;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemDiscoverableScheduleRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private String channel;
  private boolean isDiscoverable;
  private Date startDateTime;
  private Date endDateTime;

  public ItemDiscoverableScheduleRequest() {

  }

  public ItemDiscoverableScheduleRequest(String channel, boolean isDiscoverable,
      Date startDateTime, Date endDateTime) {
    this.channel = channel;
    this.isDiscoverable = isDiscoverable;
    this.startDateTime = startDateTime;
    this.endDateTime = endDateTime;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
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

  public boolean isDiscoverable() {
    return this.isDiscoverable;
  }

  public void setChannel(String channel) {
    this.channel = channel;
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
            "ItemDiscoverableScheduleDTO [channel=%s, isDiscoverable=%s, startDateTime=%s, endDateTime=%s, toString()=%s]",
            this.channel, this.isDiscoverable, this.startDateTime, this.endDateTime,
            super.toString());
  }
}
