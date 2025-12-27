package com.gdn.mta.product.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import java.util.Date;

import com.gdn.GdnBaseEntity;

@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = LogAuditTrailUpdatedOfflineProduct.TABLE_NAME)
public class LogAuditTrailUpdatedOfflineProduct extends GdnBaseEntity {
  public static final String TABLE_NAME = "PRD_LOG_AUDIT_TRAIL_UPDATED_OFFLINE_PRODUCT";
  private static final String ACCESS_TIME = "access_time";
  private static final String ACTIVITY = "activity";
  private static final String BUSINESS_PARTNER_CODE = "business_partner_code";
  private static final String CHANGED_BY = "changed_by";
  private static final String CLIENT_HOST = "client_host";
  private static final String ITEM_SKU = "item_sku";
  private static final String PICKUP_POINT_CODE = "pickup_point_code";
  private static final String NEW_VALUES = "new_values";
  private static final String OLD_VALUES = "old_values";
  private static final String REQUEST_ID = "request_id";

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = ACCESS_TIME)
  private Date accessTime;

  @Column(name = ACTIVITY)
  private String activity;

  @Column(name = BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = CHANGED_BY)
  private String changedBy;

  @Column(name = CLIENT_HOST)
  private String clientHost;

  @Column(name = ITEM_SKU)
  private String itemSku;

  @Column(name = PICKUP_POINT_CODE)
  private String pickupPointCode;

  @Column(name = NEW_VALUES)
  private String newValues;

  @Column(name = OLD_VALUES)
  private String oldValues;

  @Column(name = REQUEST_ID)
  private String requestId;

}

