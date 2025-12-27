package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueDomainEventModel {

  private String allowedAttributeCode;
  private String value;
  private Integer sequence;

  public AllowedAttributeValueDomainEventModel() {
    // do nothing
  }

  public AllowedAttributeValueDomainEventModel(String allowedAttributeCode, String value, Integer sequence) {
    super();
    this.allowedAttributeCode = allowedAttributeCode;
    this.value = value;
    this.sequence = sequence;
  }

  public String getAllowedAttributeCode() {
    return allowedAttributeCode;
  }

  public Integer getSequence() {
    return sequence;
  }

  public String getValue() {
    return value;
  }

  public void setAllowedAttributeCode(String allowedAttributeCode) {
    this.allowedAttributeCode = allowedAttributeCode;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setValue(String value) {
    this.value = value;
  }

  @Override
  public String toString() {
    return String
        .format(
            "AllowedAttributeValueDomainEventModel [allowedAttributeCode=%s, value=%s, sequence=%s, getAllowedAttributeCode()=%s, getValue()=%s, getSequence()=%s]",
            allowedAttributeCode, value, sequence, getAllowedAttributeCode(), getValue(), getSequence());
  }

}
