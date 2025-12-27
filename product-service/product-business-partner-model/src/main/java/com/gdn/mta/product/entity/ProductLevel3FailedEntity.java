package com.gdn.mta.product.entity;


import com.gdn.GdnBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = ProductLevel3FailedEntity.TABLE_NAME)
public class ProductLevel3FailedEntity extends GdnBaseEntity {

  public static final String TABLE_NAME = "PRD_PRODUCT_LEVEL_3_CREATION_RETRY";
  public static final String COLUMN_PRODUCT_SKU = "PRODUCT_SKU";
  public static final String COLUMN_RETRY_STATUS = "RETRY_STATUS";
  public static final String COLUMN_RETRY_COUNT = "RETRY_COUNT";

  @Column(name = COLUMN_PRODUCT_SKU, nullable = false)
  private String productSku;

  @Column(name = COLUMN_RETRY_STATUS, nullable = false)
  private String retryStatus;

  @Column(name = COLUMN_RETRY_COUNT, nullable = false)
  private int retryCount;

}
