package com.gdn.mta.product.entity;

import org.apache.commons.lang3.builder.ToStringBuilder;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;

/**
 * Created by Kesha on 01/08/16.
 */
@Entity
@Table(name = ProductFLow2Audit.TABLE_NAME)
public class ProductFLow2Audit extends GdnBaseEntity {
  public static final String TABLE_NAME = "PRD_PRODUCT_FLOW2_AUDIT";
  private static final String COLUMN_STATUS = "STATUS";
  private static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String COLUMN_NOTES = "NOTES";
  private static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = COLUMN_NOTES)
  private String notes;

  @Column(name = COLUMN_STATUS)
  private String status;

  @Column(name = COLUMN_PRODUCT_CODE)
  private String productCode;

  public ProductFLow2Audit(String businessPartnerCode, String notes, String status, String
      productCode) {
    this.businessPartnerCode = businessPartnerCode;
    this.notes = notes;
    this.status = status;
    this.productCode = productCode;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public String getNotes() {
    return notes;
  }

  public String getStatus() {
    return status;
  }

  public String getProductCode() {
    return productCode;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("businessPartnerCode", businessPartnerCode)
        .append("notes", notes)
        .append("status", status)
        .append("productCode", productCode)
        .append("createdBy", getCreatedBy())
        .append("createdDate", getCreatedDate())
        .toString();
  }
}
