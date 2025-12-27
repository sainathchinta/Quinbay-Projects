package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Entity
@Table(name = ProductCategoryArchive.TABLE_NAME)
public class ProductCategoryArchive extends GdnBaseEntity {
  public static final String TABLE_NAME = "PCC_PRODUCT_CATEGORY_ARCHIVE";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_CATEGORY_ID = "CATEGORY_ID";

  @Column(name = COLUMN_PRODUCT_ID)
  private String productId;

  @Column(name = COLUMN_CATEGORY_ID)
  private String categoryId;
}
