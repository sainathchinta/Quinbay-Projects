package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Entity
@Table(name = ProductActionRetry.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    GdnBaseEntity.STORE_ID, ProductActionRetry.COLUMN_PRODUCT_CODE, ProductActionRetry.COLUMN_ACTION})})
public class ProductActionRetry extends GdnBaseEntity {

  private static final long serialVersionUID = 2796532455125701484L;
  public static final String TABLE_NAME = "PDT_PRODUCT_ACTION_RETRY";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_ACTION = "ACTION";
  public static final String COLUMN_RETRY_COUNT = "RETRY_COUNT";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_DATA = "DATA";
  public static final String COLUMN_FAILED_REASON = "FAILED_REASON";

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_ACTION, nullable = false)
  private String action;

  @Column(name = COLUMN_RETRY_COUNT, nullable = false)
  private int retryCount;

  @Column(name = COLUMN_DATA)
  private String data;

  @Column(name = COLUMN_FAILED_REASON)
  private String failedReason;

  @Column(name = COLUMN_STATUS, nullable = false)
  @Enumerated(EnumType.STRING)
  private ActionRetryStatus status;

}
