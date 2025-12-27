package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper=true)
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = ProductScoreUpdate.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {Product.COLUMN_PRODUCT_CODE})})
public class ProductScoreUpdate extends GdnBaseEntity {

  private static final long serialVersionUID = 2976190127066616840L;
  public static final String TABLE_NAME = "PCC_PRODUCT_SCORE_UPDATE";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_UPDATED = "IS_UPDATED";

  @Column(name = ProductScoreUpdate.COLUMN_PRODUCT_CODE)
  private String productCode;

  @Column(name = ProductScoreUpdate.COLUMN_UPDATED)
  private boolean updated = false;

  @Override
  public String toString() {
    return "ProductScoreUpdate{" + "productCode='" + productCode + '\'' + ", updated=" + updated + '}';
  }
}
