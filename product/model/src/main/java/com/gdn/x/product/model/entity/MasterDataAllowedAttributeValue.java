package com.gdn.x.product.model.entity;

import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class MasterDataAllowedAttributeValue implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.ALLOWED_ATTRIBUTE_VALUE_CODE)
  private String allowedAttributeValueCode;

  @Field(value = ProductFieldNames.ATTRIBUTE_VALUE)
  private String value;

  @Field(value = ProductFieldNames.SEQUENCE)
  private Integer sequence;

  @Transient
  private String valueType;

  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }

  public MasterDataAllowedAttributeValue() {

  }

  public MasterDataAllowedAttributeValue(String allowedAttributeValueCode, String value,
      int sequence) {
    super();
    this.allowedAttributeValueCode = allowedAttributeValueCode;
    this.value = value;
    this.sequence = sequence;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getAllowedAttributeValueCode() {
    return this.allowedAttributeValueCode;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public String getValue() {
    return this.value;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAllowedAttributeValueCode(String allowedAttributeValueCode) {
    this.allowedAttributeValueCode = allowedAttributeValueCode;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String.format(
        "MasterDataAllowedAttributeValue [allowedAttributeValueCode=%s, value=%s, sequence=%s, toString()=%s]",
        this.allowedAttributeValueCode, this.value, this.sequence, super.toString());
  }
}
