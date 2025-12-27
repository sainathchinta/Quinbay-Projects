package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

@Entity
@Table(name = ProductOptionalParameter.TABLE_NAME)
public class ProductOptionalParameter extends GdnBaseEntity {
  private static final long serialVersionUID = 642363933190865286L;
  public static final String TABLE_NAME = "PCC_PRODUCT_OPTIONAL_PARAMETER";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_NAME = "NAME";
  public static final String COLUMN_UNIQUE = "\"UNIQUE\"";

  @Column(name = ProductOptionalParameter.COLUMN_PRODUCT_CODE)
  private String productCode;

  @Column(name = ProductOptionalParameter.COLUMN_NAME)
  private String name;

  @Column(name = ProductOptionalParameter.COLUMN_UNIQUE)
  private boolean unique = false;

  public ProductOptionalParameter() {}

  public ProductOptionalParameter(String productCode, String name, boolean unique, String storeId) {
    this.productCode = productCode;
    this.name = name;
    this.unique = unique;
    this.setStoreId(storeId);
    this.setMarkForDelete(false);
  }

  public String getName() {
    return this.name;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public boolean isUnique() {
    return this.unique;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setUnique(boolean unique) {
    this.unique = unique;
  }

  @Override
  public String toString() {
    return String.format("ProductOptionalParameter [productCode=%s, name=%s, unique=%s, toString()=%s]",
        this.productCode, this.name, this.unique, super.toString());
  }
}
