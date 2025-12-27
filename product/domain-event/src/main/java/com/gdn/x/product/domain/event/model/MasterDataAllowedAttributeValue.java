package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataAllowedAttributeValue implements Serializable {

  private static final long serialVersionUID = -8606000602351793428L;

  private String allowedAttributeValueCode;

  private String value;

  private Integer sequence;

  public MasterDataAllowedAttributeValue() {

  }

  public MasterDataAllowedAttributeValue(String allowedAttributeValueCode, String attributeValue,
      Integer sequence) {
    super();
    this.allowedAttributeValueCode = allowedAttributeValueCode;
    this.value = attributeValue;
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

  public void setValue(String attributeValue) {
    this.value = attributeValue;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataAllowedAttributeValue [allowedAttributeValueCode=%s, attributeValue=%s, sequence=%s, toString()=%s]",
            this.allowedAttributeValueCode, this.value, this.sequence, super.toString());
  }
}
