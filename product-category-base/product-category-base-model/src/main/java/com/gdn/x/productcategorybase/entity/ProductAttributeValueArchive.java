package com.gdn.x.productcategorybase.entity;

import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
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
@Table(name = ProductAttributeValueArchive.TABLE_NAME)
public class ProductAttributeValueArchive extends GdnBaseEntity {
  public static final String TABLE_NAME = "PCC_PRODUCT_ATTRIBUTE_VALUE_ARCHIVE";
  public static final String COLUMN_ALLOWED_ATTRIBUTE_VALUE_ID = "ALLOWED_ATTRIBUTE_VALUE_ID";
  public static final String COLUMN_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID = "PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID";
  public static final String COLUMN_PRODUCT_ATTRIBUTE_DESCRIPTIVE_VALUE = "DESCRIPTIVE_VALUE";
  public static final String COLUMN_DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE = "DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE";
  public static final String COLUMN_PRODUCT_ATTRIBUTE_ID = "PRODUCT_ATTRIBUTE_ID";

  @Column(name = COLUMN_PRODUCT_ATTRIBUTE_ID)
  private String productAttributeId;

  @Column(name = COLUMN_ALLOWED_ATTRIBUTE_VALUE_ID)
  private String allowedAttributeValueId;

  @Column(name = COLUMN_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID)
  private String predefinedAllowedAttributeValueId;

  @Column(name = COLUMN_PRODUCT_ATTRIBUTE_DESCRIPTIVE_VALUE)
  private String descriptiveAttributeValue;

  @Enumerated(EnumType.STRING)
  @Column(name = COLUMN_DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE)
  private DescriptiveAttributeValueType descriptiveAttributeValueType;
}
