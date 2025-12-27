package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import org.apache.commons.lang3.builder.ToStringBuilder;

@Entity
@Table(name = MerchantConfiguration.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {MerchantConfiguration.COLUMN_MERCHANT_CODE, GdnBaseEntity.STORE_ID})})
public class MerchantConfiguration extends GdnBaseEntity {

  private static final long serialVersionUID = -1350948088869600308L;

  public static final String TABLE_NAME = "PCC_MERCHANT_CONFIGURATION";
  public static final String COLUMN_MERCHANT_CODE = "MERCHANT_CODE";
  private static final String COLUMN_MERCHANT_NAME = "MERCHANT_NAME";
  private static final String COLUMN_CATEGORY_NAME = "CATEGORY_NAME";
  private static final String COLUMN_REVIEW_CONFIG = "REVIEW_CONFIG";

  @Column(name = MerchantConfiguration.COLUMN_MERCHANT_CODE)
  private String merchantCode;

  @Column(name = MerchantConfiguration.COLUMN_MERCHANT_NAME)
  private String merchantName;

  @Column(name = MerchantConfiguration.COLUMN_CATEGORY_NAME)
  private String categoryName;

  @Column(name = MerchantConfiguration.COLUMN_REVIEW_CONFIG)
  private String reviewConfig;

  public MerchantConfiguration() {
  }

  public MerchantConfiguration(String merchantCode, String merchantName, String categoryName, String reviewConfig) {
    this.merchantCode = merchantCode;
    this.merchantName = merchantName;
    this.categoryName = categoryName;
    this.reviewConfig = reviewConfig;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public String getMerchantName() {
    return merchantName;
  }

  public void setMerchantName(String merchantName) {
    this.merchantName = merchantName;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public String getReviewConfig() {
    return reviewConfig;
  }

  public void setReviewConfig(String reviewConfig) {
    this.reviewConfig = reviewConfig;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).
        append("merchantCode", merchantCode).
        append("merchantName", merchantName).
        append("categoryName", categoryName).
        append("reviewConfig", reviewConfig).
        toString();
  }
}
