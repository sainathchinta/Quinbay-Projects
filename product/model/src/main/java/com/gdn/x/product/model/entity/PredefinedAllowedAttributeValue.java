package com.gdn.x.product.model.entity;

import org.apache.commons.lang3.StringUtils;

import com.gdn.x.product.enums.ProductFieldNames;
import jakarta.persistence.Column;
import jakarta.persistence.Transient;

public class PredefinedAllowedAttributeValue implements GdnBaseEmbedded {

  private static final long serialVersionUID = -1435353917036838635L;

  @Column(name = ProductFieldNames.PREDEFINED_ALLOWED_ATTRIBUTE_VALUE_CODE)
  private String predefinedAllowedAttributeCode;

  @Column(name = ProductFieldNames.VALUE)
  private String value;

  @Column(name = ProductFieldNames.SEQUENCE)
  private Integer sequence;

  @Transient
  private String valueEn;

  @Transient
  private String attributeType = StringUtils.EMPTY;

  public PredefinedAllowedAttributeValue() {}

  public PredefinedAllowedAttributeValue(String predefinedAllowedAttributeCode, String value,
      Integer sequence) {
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
    this.value = value;
    this.sequence = sequence;
  }

  public String getPredefinedAllowedAttributeCode() {
    return this.predefinedAllowedAttributeCode;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public String getValue() {
    return this.value;
  }

  public void setPredefinedAllowedAttributeCode(String predefinedAllowedAttributeCode) {
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public String getValueEn() {
    return valueEn;
  }

  public void setValueEn(String valueEn) {
    this.valueEn = valueEn;
  }

  public String getAttributeType() {
    return attributeType;
  }

  public void setAttributeType(String attributeType) {
    this.attributeType = attributeType;
  }

  @Override
  public String toString() {
    return String
        .format(
            "PredefinedAllowedAttributeValue [predefinedAllowedAttributeCode=%s, value=%s, sequence=%s, toString()=%s]",
            predefinedAllowedAttributeCode, value, sequence, super.toString());
  }
}
