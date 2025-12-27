package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@Data
@ToString
@Entity
@Table(name = ProductMigration.TABLE_NAME)
public class ProductMigration extends GdnBaseEntity {
  private static final long serialVersionUID = 2059382338969212910L;
  public static final String TABLE_NAME = "PRD_PRODUCT_MIGRATION";
  public static final String PRODUCT_ID = "PRODUCT_ID";
  public static final String GDN_PRODUCT_SKU = "GDN_PRODUCT_SKU";
  public static final String BUSINESS_PARTNER_ID = "BUSINESS_PARTNER_ID";
  public static final String BUSINESS_PARTNER_NAME = "BUSINESS_PARTNER_NAME";
  public static final String PRODUCT_CODE = "PRODUCT_CODE";
  public static final String MIGRATED_PRODUCT_CODE = "MIGRATED_PRODUCT_CODE";
  public static final String MIGRATED_PRODUCT_ID = "MIGRATED_PRODUCT_ID";
  public static final String MERCHANT_STATUS = "MERCHANT_STATUS";
  public static final String MERCHANT_TYPE = "MERCHANT_TYPE";
  public static final String MIGRATION_STATUS = "MIGRATION_STATUS";
  public static final String SYNC_STATUS = "SYNC_STATUS";
  public static final String L3_UPDATED = "L3_UPDATED";
  public static final String PRODUCT_PUBLISHED = "PRODUCT_PUBLISHED";
  public static final String RETRY_COUNT = "RETRY_COUNT";

  @Column(name = PRODUCT_ID)
  private String productId;

  @Column(name = GDN_PRODUCT_SKU)
  private String gdnProductSku;

  @Column(name = BUSINESS_PARTNER_ID)
  private String businessPartnerId;

  @Column(name = BUSINESS_PARTNER_NAME)
  private String businessPartnerName;

  @Column(name = PRODUCT_CODE)
  private String productCode;

  @Column(name = MIGRATED_PRODUCT_CODE)
  private String migratedProductCode;

  @Column(name = MIGRATED_PRODUCT_ID)
  private String migratedProductId;

  @Column(name = MERCHANT_STATUS)
  private String merchantStatus;

  @Column(name = MERCHANT_TYPE)
  private String merchantType;

  @Column(name = MIGRATION_STATUS)
  private String migrationStatus;

  @Column(name = SYNC_STATUS)
  private boolean syncStatus;

  @Column(name = L3_UPDATED)
  private boolean l3Updated;

  @Column(name = PRODUCT_PUBLISHED)
  private boolean productPublished;

  @Column(name = RETRY_COUNT)
  private int retryCount;
}
