package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.Date;

import jakarta.persistence.Access;
import jakarta.persistence.AccessType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import org.hibernate.annotations.GenericGenerator;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
@Entity
@Table(name = LogAuditTrailUpdatedProductBackup.TABLE_NAME)
@Access(value = AccessType.FIELD)
public class LogAuditTrailUpdatedProductBackup implements Serializable {

  private static final long serialVersionUID = -5460869794399036803L;
  public static final String TABLE_NAME = "PRD_LOG_AUDIT_TRAIL_UPDATED_PRODUCT_BACKUP";
  public static final String COLUMN_AUDIT_TRAIL_ID = "AUDIT_TRAIL_ID";
  public static final String COLUMN_PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  public static final String COLUMN_ONLINE_STATUS = "ONLINE_STATUS";

  @Id
  @Column(name = COLUMN_AUDIT_TRAIL_ID)
  @GeneratedValue(generator = "system-uuid")
  @GenericGenerator(name = "system-uuid", strategy = "uuid2")
  private String pkLogAuditTrail;

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

  @Column(name = "ACTIVATED_DATE")
  private Date activatedDate;

  @Column(name = "REQUEST_ID")
  private String requestId;

  @Column(name = "ACCESS_CHANNEL")
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

}
