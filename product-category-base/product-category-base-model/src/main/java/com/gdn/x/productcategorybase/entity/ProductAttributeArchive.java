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
@Table(name = ProductAttributeArchive.TABLE_NAME)
public class ProductAttributeArchive extends GdnBaseEntity {
  public static final String TABLE_NAME = "PCC_PRODUCT_ATTRIBUTE_ARCHIVE";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_PRODUCT_ATTRIBUTE_NAME = "PRODUCT_ATTRIBUTE_NAME";
  public static final String COLUMN_IS_OWN_BY_PRODUCT_ITEM = "IS_OWN_BY_PRODUCT_ITEM";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";

  @Column(name = COLUMN_ATTRIBUTE_ID)
  private String attributeId;

  @Column(name = COLUMN_PRODUCT_ID)
  private String productId;

  @Column(name = COLUMN_PRODUCT_ATTRIBUTE_NAME)
  private String productAttributeName;

  @Column(name = COLUMN_IS_OWN_BY_PRODUCT_ITEM)
  private boolean isOwnByProductItem;

  @Column(name = COLUMN_SEQUENCE)
  private Integer sequence;
}
