package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class ProductSpecialAttribute implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.ATTRIBUTE_CODE)
  private String attributeCode;

  @Field(value = ProductFieldNames.ATTRIBUTE_NAME)
  private String attributeName;

  @Field(value = ProductFieldNames.ATTRIBUTE_VALUE)
  private String attributeValue;

  public ProductSpecialAttribute() {

  }

  public ProductSpecialAttribute(String attributeCode, String attributeName, String attributeValue) {
    super();
    this.attributeCode = attributeCode;
    this.attributeName = attributeName;
    this.attributeValue = attributeValue;
  }


  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getAttributeCode() {
    return this.attributeCode;
  }

  public String getAttributeName() {
    return this.attributeName;
  }

  public String getAttributeValue() {
    return this.attributeValue;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }

  public void setAttributeName(String attributeName) {
    this.attributeName = attributeName;
  }

  public void setAttributeValue(String attributeValue) {
    this.attributeValue = attributeValue;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductSpecialAttribute [attributeCode=%s, attributeName=%s, attributeValue=%s, toString()=%s]",
            this.attributeCode, this.attributeName, this.attributeValue, super.toString());
  }

}
