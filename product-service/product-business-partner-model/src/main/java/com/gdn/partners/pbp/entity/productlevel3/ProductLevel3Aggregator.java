package com.gdn.partners.pbp.entity.productlevel3;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.UniqueConstraint;

import com.gdn.GdnBaseEntity;
import com.gdn.partners.pbp.entity.eventstore.EventStore;

@Entity
@Table(name = ProductLevel3Aggregator.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(
    columnNames = {ProductLevel3Aggregator.COLUMN_GDN_SKU})})
public class ProductLevel3Aggregator extends GdnBaseEntity {

  private static final long serialVersionUID = 4000846926426613993L;
  public static final String TABLE_NAME = ""
      + "PBP_PRODUCT_LEVEL3_AGGREGATOR";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_GDN_SKU = "GDN_SKU";
  public static final String COLUMN_STATE = "STATE";
  public static final String COLUMN_OOS = "OOS";
  public static final String COLUMN_MINIMUM_STOCK = "MINIMUM_STOCK";
  public static final String COLUMN_EVENT_TIMESTAMP = "EVENT_TIMESTAMP";

  public enum ProductLevel3AggregatorInventoryCriteria {
    NONE,
    AVAILABLE,
    OOS,
    MINIMUM_STOCK
  }

  public enum ProductLevel3AggregatorState {
    ACTIVE,
    ARCHIVED
  }

  @Column(name = ProductLevel3Aggregator.COLUMN_BUSINESS_PARTNER_CODE, nullable = false)
  private String businessPartnerCode;

  @Column(name = ProductLevel3Aggregator.COLUMN_GDN_SKU, nullable = false)
  private String gdnSku;

  @Column(name = ProductLevel3Aggregator.COLUMN_STATE, nullable = false)
  @Enumerated(EnumType.STRING)
  private ProductLevel3AggregatorState state;

  @Column(name = ProductLevel3Aggregator.COLUMN_OOS, nullable = false)
  private boolean oos = false;

  @Column(name = ProductLevel3Aggregator.COLUMN_MINIMUM_STOCK, nullable = false)
  private boolean minimumStock = false;
  
  @Column(name = EventStore.COLUMN_EVENT_TIMESTAMP)
  @Temporal(value = TemporalType.TIMESTAMP)
  private Date eventTimestamp;

  public ProductLevel3Aggregator() {}

  public ProductLevel3Aggregator(String businessPartnerCode, String gdnSku, ProductLevel3AggregatorState state,
      boolean oos, boolean minimumStock) {
    this.businessPartnerCode = businessPartnerCode;
    this.gdnSku = gdnSku;
    this.state = state;
    this.oos = oos;
    this.minimumStock = minimumStock;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public ProductLevel3AggregatorState getState() {
    return state;
  }

  public void setState(ProductLevel3AggregatorState state) {
    this.state = state;
  }

  public Boolean getOos() {
    return oos;
  }

  public void setOos(Boolean oos) {
    this.oos = oos;
  }

  public Boolean getMinimumStock() {
    return minimumStock;
  }

  public void setMinimumStock(Boolean minimumStock) {
    this.minimumStock = minimumStock;
  }
  
  public Date getEventTimestamp() {
    return eventTimestamp;
  }

  public void setEventTimestamp(Date eventTimestamp) {
    this.eventTimestamp = eventTimestamp;
  }

  public void setOos(boolean oos) {
    this.oos = oos;
  }

  public void setMinimumStock(boolean minimumStock) {
    this.minimumStock = minimumStock;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductLevel3Aggregator [businessPartnerCode=%s, gdnSku=%s, state=%s, oos=%s, minimumStock=%s, eventTimestamp=%s]",
        businessPartnerCode, gdnSku, state, oos, minimumStock, eventTimestamp);
  }
}
