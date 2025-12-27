package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.GdnBaseEntity;

/**
 * Created by hardikbohra on 10/05/18.
 */
@Entity
@Table(name = CategoryUserMapping.TABLE_NAME)
public class CategoryUserMapping extends GdnBaseEntity {

  public static final String TABLE_NAME = "PRD_CATEGORY_USER_MAPPING";
  public static final String COLUMN_RECAT_ID = "RECAT_ID";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_USERNAME = "USERNAME";
  public static final String COLUMN_STATUS = "STATUS";

  @Column(name = COLUMN_RECAT_ID, nullable = false)
  private String recatId;

  @Column(name = COLUMN_CATEGORY_CODE, nullable = false)
  private String categoryCode;

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = COLUMN_USERNAME)
  private String username;

  @Column(name = COLUMN_STATUS, nullable = false)
  private String status;

  public CategoryUserMapping() {
    // default constructor
  }

  public CategoryUserMapping(String recatId, String categoryCode, String businessPartnerCode,
      String username, String status, String storeId) {
    this.recatId = recatId;
    this.categoryCode = categoryCode;
    this.businessPartnerCode = businessPartnerCode;
    this.username = username;
    this.status = status;
    this.setStoreId(storeId);
  }

  public String getRecatId() {
    return recatId;
  }

  public void setRecatId(String recatId) {
    this.recatId = recatId;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("recatId", recatId).append("categoryCode", categoryCode).append
        ("businessPartnerCode", businessPartnerCode).append("username", username).append("status", status).toString();
  }
}
