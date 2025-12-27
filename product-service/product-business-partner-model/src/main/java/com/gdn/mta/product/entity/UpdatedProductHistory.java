package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Access;
import jakarta.persistence.AccessType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.hibernate.annotations.GenericGenerator;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Table(name = UpdatedProductHistory.TABLE_NAME)
@Access(value = AccessType.FIELD)
public class UpdatedProductHistory implements Serializable {

  private static final long serialVersionUID = 1L;
  public static final String TABLE_NAME = "PRD_UPDATED_PRODUCT_HISTORY";
  public static final String COLUMN_AUDIT_TRAIL_ID = "AUDIT_TRAIL_ID";
  public static final String COLUMN_PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  public static final String COLUMN_ONLINE_STATUS = "ONLINE_STATUS";

  @Id
  @Column(name = COLUMN_AUDIT_TRAIL_ID)
  @GeneratedValue(generator = "system-uuid")
  @GenericGenerator(name = "system-uuid", strategy = "uuid2")
  private String auditTrailId;

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = "ACCESS_TIME")
  private Date accessTime;

  @Column(name = "BUSINESSPARTNER_CODE")
  private String businessPartnerCode;

  @Column(name = "CHANGEDBY")
  private String changedBy;

  @Column(name = "OLD_VALUES")
  private String oldValues;

  @Column(name = "NEW_VALUES")
  private String newValues;

  @Column(name = "STATUS")
  private Boolean status;

  @Column(name = "CLIENT_HOST")
  private String clientHost;

  @Column(name = "GDN_SKU")
  private String gdnSku;

  @Column(name = "ACTIVITY")
  private String activity;
  
  @Column(name="ACTIVATED_DATE")
  private Date activatedDate;

  @Column(name="REQUEST_ID")
  private String requestId;

  @Column(name="ACCESS_CHANNEL")
  private String accessChannel;

  @Column(name = "DESCRIPTION")
  private String description;

  @Column(name = "PRODUCT_SKU")
  private String productSku;

  @Column(name = "GDN_NAME")
  private String gdnName;

  @Column(name = COLUMN_PICKUP_POINT_CODE)
  private String pickupPointCode;

  @Column(name = COLUMN_ONLINE_STATUS)
  private Boolean onlineStatus;

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }

}
