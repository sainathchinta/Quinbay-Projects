package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataAllowedAttributeValueDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String allowedAttributeValueCode;

  private String value;

  private Integer sequence;

  private String valueType;

  public MasterDataAllowedAttributeValueDTO() {

  }

  public MasterDataAllowedAttributeValueDTO(String allowedAttributeValueCode, String value,
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

  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
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
        "MasterDataAllowedAttributeValue [allowedAttributeValueCode=%s, value=%s, sequence=%s,valueType=%s, toString()=%s]",
        this.allowedAttributeValueCode, this.value, this.sequence, this.valueType, super.toString());
  }
}
