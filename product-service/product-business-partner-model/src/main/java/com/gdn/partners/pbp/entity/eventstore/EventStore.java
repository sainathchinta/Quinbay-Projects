package com.gdn.partners.pbp.entity.eventstore;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import org.hibernate.annotations.GenericGenerator;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.gdn.partners.pbp.commons.util.CommonUtils;

@Entity
@Table(name = EventStore.TABLE_NAME)
@Inheritance(strategy = InheritanceType.JOINED)
@EntityListeners(value = {AuditingEntityListener.class})
public abstract class EventStore {
  public static final String TABLE_NAME = "event_store";
  public static final String COLUMN_ID = "id";
  public static final String COLUMN_STORE_ID = "store_id";
  public static final String COLUMN_EVENT_NAME = "event_name";
  public static final String COLUMN_EVENT_TIMESTAMP = "event_timestamp";
  public static final String COLUMN_STORED_DATE = "stored_date";

  @Id
  @Column(name = EventStore.COLUMN_ID)
  @GeneratedValue(generator = "system-uuid")
  @GenericGenerator(name = "system-uuid", strategy = "uuid2")
  @org.springframework.data.annotation.Id
  private String id;

  @Column(name = EventStore.COLUMN_STORE_ID, nullable = false)
  private String storeId;

  @Column(name = EventStore.COLUMN_EVENT_NAME)
  private String eventName;

  @Column(name = EventStore.COLUMN_EVENT_TIMESTAMP, nullable = false)
  @Temporal(value = TemporalType.TIMESTAMP)
  private Date eventTimestamp;

  @Column(name = EventStore.COLUMN_STORED_DATE)
  @Temporal(value = TemporalType.TIMESTAMP)
  @CreatedDate
  private Date storedDate;

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getEventName() {
    return eventName;
  }

  public void setEventName(String eventName) {
    this.eventName = eventName;
  }

  public Date getEventTimestamp() {
    return eventTimestamp;
  }

  public void setEventTimestamp(Date eventTimestamp) {
    this.eventTimestamp = eventTimestamp;
  }

  public Date getStoredDate() {
    return storedDate;
  }

  public void setStoredDate(Date storedDate) {
    this.storedDate = storedDate;
  }

  @Override
  public String toString() {
    return CommonUtils.stringifyBean(this);
  }
}
