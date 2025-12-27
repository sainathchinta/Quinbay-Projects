package com.gdn.mta.product.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;
import com.gdn.mta.product.enums.SuspensionStatus;

@Entity
@Table(name = ProductSuspensionHistory.TABLE_NAME)
public class ProductSuspensionHistory extends GdnBaseEntity {

  private static final long serialVersionUID = 6840286436827900530L;

  public static final String TABLE_NAME = "PRD_PRODUCT_SUSPENSION_HISTORY";
  public static final String COLUMN_PRODUCT_SKU = "PRODUCT_SKU";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_REASON = "REASON";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_COUNT = "COUNT";

  @Column(name = COLUMN_PRODUCT_SKU, nullable = false)
  private String productSku;

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE, nullable = false)
  private String businessPartnerCode;

  @Column(name = COLUMN_STATUS, nullable = false)
  @Enumerated(EnumType.STRING)
  private SuspensionStatus status;

  @Column(name = COLUMN_REASON, nullable = false)
  private String reason;

  @Column(name = COLUMN_DESCRIPTION, nullable = false)
  private String description;

  @Column(name = COLUMN_COUNT)
  private int count = 0;

  public ProductSuspensionHistory() {}

  public ProductSuspensionHistory(String productSku, String businessPartnerCode, String reason, String description, SuspensionStatus status, int count,
      String createdBy, Date createdDate, String storeId) {
    super();
    this.productSku = productSku;
    this.businessPartnerCode = businessPartnerCode;
    this.reason = reason;
    this.description = description;
    this.status = status;
    this.count = count;
    setCreatedBy(createdBy);
    setCreatedDate(createdDate);
    setStoreId(storeId);
    setUpdatedBy(createdBy);
    setUpdatedDate(createdDate);
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public SuspensionStatus getStatus() {
    return status;
  }

  public void setStatus(SuspensionStatus status) {
    this.status = status;
  }

  public String getReason() {
    return reason;
  }

  public void setReason(String reason) {
    this.reason = reason;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public int getCount() {
    return count;
  }

  public void setCount(int count) {
    this.count = count;
  }

  @Override
  public String toString() {
    return "ProductSuspensionHistory{" + "productSku='" + productSku + '\'' + ", businessPartnerCode='"
        + businessPartnerCode + '\'' + ", status=" + status + ", reason='" + reason + '\'' + ", description='"
        + description + '\'' + ", count=" + count + '}';
  }
}