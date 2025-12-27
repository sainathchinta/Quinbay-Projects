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
@Table(name = ProductItemAttributeValueArchive.TABLE_NAME)
public class ProductItemAttributeValueArchive extends GdnBaseEntity {
  public static final String TABLE_NAME = "PCC_PRODUCT_ITEM_ATTRIBUTE_VALUE_ARCHIVE";
  public static final String COLUMN_PRODUCT_ITEM_ID = "PRODUCT_ITEM_ID";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_VALUE = "VALUE";

  @Column(name = COLUMN_PRODUCT_ITEM_ID)
  private String productItemId;

  @Column(name = COLUMN_ATTRIBUTE_ID)
  private String attributeId;

  @Column(name = COLUMN_VALUE)
  private String value;
}
