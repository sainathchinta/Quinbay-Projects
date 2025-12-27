package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = ProductCategory.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {ProductCategory.COLUMN_PRODUCT_ID, ProductCategory.COLUMN_CATEGORY_ID})})
public class ProductCategory extends GdnBaseEntity {

  private static final long serialVersionUID = -6714415199144077868L;
  public static final String TABLE_NAME = "PCC_PRODUCT_CATEGORY";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_CATEGORY_ID = "CATEGORY_ID";

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductCategory.COLUMN_PRODUCT_ID)
  private Product product;

  @Column(name = ProductCategory.COLUMN_PRODUCT_ID, insertable = false, updatable = false)
  private String productId;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductCategory.COLUMN_CATEGORY_ID)
  private Category category;

  @Column(name = ProductCategory.COLUMN_CATEGORY_ID, insertable = false, updatable = false)
  private String categoryId;

  public ProductCategory(Product product, Category category, String storeId) {
    this.product = product;
    this.category = category;
    this.setStoreId(storeId);
  }

  @Override
  public String toString() {
    return String.format("ProductCategory [product=%s, category=%s, toString()=%s]", this.product, this.category,
        super.toString());
  }
}
