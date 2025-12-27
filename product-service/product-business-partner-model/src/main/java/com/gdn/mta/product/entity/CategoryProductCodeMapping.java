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
@Table(name = CategoryProductCodeMapping.TABLE_NAME)
public class CategoryProductCodeMapping extends GdnBaseEntity {

  public static final String TABLE_NAME = "PRD_CATEGORY_PRODUCT_CODE_MAPPING";
  public static final String COLUMN_RECAT_ID = "RECAT_ID";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_STATUS = "STATUS";

  @Column(name = COLUMN_RECAT_ID, nullable = false)
  private String recatId;

  @Column(name = COLUMN_CATEGORY_CODE, nullable = false)
  private String categoryCode;

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_STATUS, nullable = false)
  private String status;

  public CategoryProductCodeMapping() {
    // default constructor
  }

  public CategoryProductCodeMapping(String recatId, String categoryCode, String productCode,
      String status, String storeId) {
    this.recatId = recatId;
    this.categoryCode = categoryCode;
    this.productCode = productCode;
    this.status = status;
    setStoreId(storeId);
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

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
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
        ("productCode", productCode).append("status", status).toString();
  }
}
