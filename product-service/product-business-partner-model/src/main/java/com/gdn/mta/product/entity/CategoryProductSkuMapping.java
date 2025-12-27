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
@Table(name = CategoryProductSkuMapping.TABLE_NAME)
public class CategoryProductSkuMapping extends GdnBaseEntity {

  public static final String TABLE_NAME = "PRD_CATEGORY_PRODUCT_SKU_MAPPING";
  private static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  private static final String COLUMN_PRODUCT_SKU = "PRODUCT_SKU";
  private static final String COLUMN_RECAT_ID = "RECAT_ID";
  private static final String COLUMN_STATUS = "STATUS";

  @Column(name = COLUMN_RECAT_ID, nullable = false)
  private String recatId;

  @Column(name = COLUMN_CATEGORY_CODE, nullable = false)
  private String categoryCode;

  @Column(name = COLUMN_PRODUCT_SKU, nullable = false)
  private String productSku;

  @Column(name = COLUMN_STATUS, nullable = false)
  private String status;

  public CategoryProductSkuMapping() {
    // default constructor
  }

  public CategoryProductSkuMapping(String recatId, String categoryCode, String productSku,
      String status, String storeId) {
    this.recatId = recatId;
    this.categoryCode = categoryCode;
    this.productSku = productSku;
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

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
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
        ("productSku", productSku).append("status", status).toString();
  }
}
