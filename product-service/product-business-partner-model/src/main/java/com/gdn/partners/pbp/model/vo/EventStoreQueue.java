package com.gdn.partners.pbp.model.vo;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class EventStoreQueue<T> implements Serializable {
  private static final long serialVersionUID = 7499084543727186792L;
  private String eventId;
  private String eventName;
  private boolean forIndexing;

  private T eventModel;

  public EventStoreQueue() {}

  public EventStoreQueue(T eventModel) {
    this.eventModel = eventModel;
  }

  public EventStoreQueue(T eventModel, String eventName, String eventId) {
    this.eventModel = eventModel;
    this.eventName = eventName;
    this.eventId = eventId;
  }

  public T getEventModel() {
    return eventModel;
  }

  public void setEventModel(T eventModel) {
    this.eventModel = eventModel;
  }

  public String getEventName() {
    return eventName;
  }

  public void setEventName(String eventName) {
    this.eventName = eventName;
  }

  public String getEventId() {
    return eventId;
  }

  public void setEventId(String eventId) {
    this.eventId = eventId;
  }

  public boolean isForIndexing() {
    return forIndexing;
  }

  public void setForIndexing(boolean forIndexing) {
    this.forIndexing = forIndexing;
  }

  @Override
  public String toString() {
    return "EventStore [eventName=" + eventName + ", eventModel=" + eventModel + "]";
  }
}
