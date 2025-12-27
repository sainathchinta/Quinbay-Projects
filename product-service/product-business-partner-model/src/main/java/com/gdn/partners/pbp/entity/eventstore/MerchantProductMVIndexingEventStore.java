package com.gdn.partners.pbp.entity.eventstore;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import org.hibernate.annotations.GenericGenerator;

@Entity
@Table(name = MerchantProductMVIndexingEventStore.TABLE_NAME)
public class MerchantProductMVIndexingEventStore {
  public static final String TABLE_NAME = "merchant_product_mv_indexing_event_store";
  public static final String COLUMN_ID = "id";
  public static final String COLUMN_EVENT_ID = "event_id";
  public static final String COLUMN_INDEXED_DATE = "indexed_date";
  public static final String COLUMN_INDEXED = "indexed";

  @Id
  @Column(name = MerchantProductMVIndexingEventStore.COLUMN_ID)
  @GeneratedValue(generator = "system-uuid")
  @GenericGenerator(name = "system-uuid", strategy = "uuid2")
  @org.springframework.data.annotation.Id
  private String id;

  @OneToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = MerchantProductMVIndexingEventStore.COLUMN_EVENT_ID)
  private EventStore event;

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = MerchantProductMVIndexingEventStore.COLUMN_INDEXED_DATE)
  private Date indexedDate;

  @Column(name = MerchantProductMVIndexingEventStore.COLUMN_INDEXED)
  private boolean indexed;

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public EventStore getEvent() {
    return event;
  }

  public void setEvent(EventStore event) {
    this.event = event;
  }

  public Date getIndexedDate() {
    return indexedDate;
  }

  public void setIndexedDate(Date indexedDate) {
    this.indexedDate = indexedDate;
  }

  public boolean isIndexed() {
    return indexed;
  }

  public void setIndexed(boolean indexed) {
    this.indexed = indexed;
  }

  @Override
  public String toString() {
    return "MerchantProductMVIndexingEventStore [id=" + id + ", event=" + event + ", indexedDate="
        + indexedDate + ", indexed=" + indexed + "]";
  }
}
