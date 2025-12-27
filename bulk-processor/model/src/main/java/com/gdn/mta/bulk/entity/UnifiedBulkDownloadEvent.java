package com.gdn.mta.bulk.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@EqualsAndHashCode(callSuper=true)
@Table(name = UnifiedBulkDownloadEvent.TABLE_NAME)
public class UnifiedBulkDownloadEvent extends GdnBaseEntity {

  public static final String TABLE_NAME = "BLP_UNIFIED_BULK_DOWNLOAD_EVENT";
  private static final long serialVersionUID = -9045087010976450172L;
  private static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String COLUMN_BRAND_UPDATED = "BRAND_UPDATED";
  private static final String COLUMN_PICKUP_POINT_UPDATED = "PICKUP_POINT_UPDATED";
  private static final String COLUMN_LAST_DOWNLOADED_TIME = "LAST_DOWNLOADED_TIME";
  private static final String COLUMN_DOWNLOAD_STATUS = "DOWNLOAD_STATUS";

  @Column(name = UnifiedBulkDownloadEvent.COLUMN_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = UnifiedBulkDownloadEvent.COLUMN_BRAND_UPDATED)
  private boolean brandUpdated;

  @Column(name = UnifiedBulkDownloadEvent.COLUMN_PICKUP_POINT_UPDATED)
  private boolean pickupPointUpdated;

  @Column(name = UnifiedBulkDownloadEvent.COLUMN_LAST_DOWNLOADED_TIME)
  private Date lastDownloadedTime;

  @Column(name = UnifiedBulkDownloadEvent.COLUMN_DOWNLOAD_STATUS)
  private String downloadStatus;
}