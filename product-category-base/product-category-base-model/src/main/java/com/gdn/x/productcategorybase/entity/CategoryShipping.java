package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

@Entity
@Table(name = CategoryShipping.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID,
    CategoryShipping.COLUMN_CATEGORY_CODE, CategoryShipping.COLUMN_SHIPPING_CODE})})
public class CategoryShipping extends GdnBaseEntity {

  private static final long serialVersionUID = 5829771324417752276L;

  public static final String TABLE_NAME = "PCC_CATEGORY_SHIPPING_CODE";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_SHIPPING_CODE = "SHIPPING_CODE";

  @Column(name = CategoryShipping.COLUMN_CATEGORY_CODE)
  private String categoryCode;

  @Column(name = CategoryShipping.COLUMN_SHIPPING_CODE)
  private String shippingCode;

  public CategoryShipping() {}

  public CategoryShipping(String storeId, String categoryCode, String shippingCode) {
    this.setStoreId(storeId);
    this.categoryCode = categoryCode;
    this.shippingCode = shippingCode;
  }

  public String getCategoryCode() {
    return this.categoryCode;
  }

  public String getShippingCode() {
    return this.shippingCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setShippingCode(String shippingCode) {
    this.shippingCode = shippingCode;
  }

  @Override
  public String toString() {
    return String.format("CategoryShipping [categoryCode=%s, shippingCode=%s, toString()=%s]", this.categoryCode,
        this.shippingCode, super.toString());
  }

}
