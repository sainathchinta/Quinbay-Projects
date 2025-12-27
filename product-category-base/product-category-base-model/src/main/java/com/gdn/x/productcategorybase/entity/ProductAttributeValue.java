package com.gdn.x.productcategorybase.entity;

import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = ProductAttributeValue.TABLE_NAME)
public class ProductAttributeValue extends GdnBaseEntity {

  private static final long serialVersionUID = -8632328725933563638L;
  public static final String TABLE_NAME = "PCC_PRODUCT_ATTRIBUTE_VALUE";
  public static final String COLUMN_ALLOWED_ATTRIBUTE_VALUE_ID = "ALLOWED_ATTRIBUTE_VALUE_ID";
  public static final String COLUMN_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID = "PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID";
  public static final String COLUMN_PRODUCT_ATTRIBUTE_DESCRIPTIVE_VALUE = "DESCRIPTIVE_VALUE";
  public static final String COLUMN_DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE = "DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE";
  public static final String COLUMN_PRODUCT_ATTRIBUTE_ID = "PRODUCT_ATTRIBUTE_ID";

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductAttributeValue.COLUMN_PRODUCT_ATTRIBUTE_ID)
  private ProductAttribute productAttribute;

  @Column(name = ProductAttributeValue.COLUMN_PRODUCT_ATTRIBUTE_ID, insertable = false, updatable = false)
  private String productAttributeId;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductAttributeValue.COLUMN_ALLOWED_ATTRIBUTE_VALUE_ID)
  private AllowedAttributeValue allowedAttributeValue;

  @Column(name = ProductAttributeValue.COLUMN_ALLOWED_ATTRIBUTE_VALUE_ID, insertable = false, updatable = false)
  private String allowedAttributeValueId;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductAttributeValue.COLUMN_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID)
  private PredefinedAllowedAttributeValue predefinedAllowedAttributeValue;

  @Column(name = ProductAttributeValue.COLUMN_PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_ID, insertable = false, updatable = false)
  private String predefinedAllowedAttributeValueId;

  @Column(name = ProductAttributeValue.COLUMN_PRODUCT_ATTRIBUTE_DESCRIPTIVE_VALUE)
  private String descriptiveAttributeValue;

  @Enumerated(EnumType.STRING)
  @Column(name = ProductAttributeValue.COLUMN_DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE)
  private DescriptiveAttributeValueType descriptiveAttributeValueType;

  public ProductAttributeValue(ProductAttribute productAttribute, AllowedAttributeValue allowedAttributeValue,
      String descriptiveAttributeValue, DescriptiveAttributeValueType descriptiveAttributeValueType) {
    this.productAttribute = productAttribute;
    this.allowedAttributeValue = allowedAttributeValue;
    this.descriptiveAttributeValue = descriptiveAttributeValue;
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductAttributeValue [allowedAttributeValue=%s, predefinedAllowedAttributeValue=%s, descriptiveAttributeValue=%s, descriptiveAttributeValueType=%s]",
        this.allowedAttributeValue, this.predefinedAllowedAttributeValue, this.descriptiveAttributeValue,
        this.descriptiveAttributeValueType);
  }
}
