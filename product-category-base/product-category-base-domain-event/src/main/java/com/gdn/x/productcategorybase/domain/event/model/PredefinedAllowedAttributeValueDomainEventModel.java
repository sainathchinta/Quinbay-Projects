package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PredefinedAllowedAttributeValueDomainEventModel {

  private String predefinedAllowedAttributeCode;
  private String value;
  private Integer sequence;

  public PredefinedAllowedAttributeValueDomainEventModel() {
    // do nothing
  }

  public PredefinedAllowedAttributeValueDomainEventModel(String predefinedAllowedAttributeCode, String value,
      Integer sequence) {
    super();
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
    this.value = value;
    this.sequence = sequence;
  }

  public String getPredefinedAllowedAttributeCode() {
    return predefinedAllowedAttributeCode;
  }

  public Integer getSequence() {
    return sequence;
  }

  public String getValue() {
    return value;
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

  @Override
  public String toString() {
    return String
        .format(
            "PredefinedAllowedAttributeValueDomainEventModel [predefinedAllowedAttributeCode=%s, value=%s, sequence=%s, getPredefinedAllowedAttributeCode()=%s, getValue()=%s, getSequence()=%s]",
            predefinedAllowedAttributeCode, value, sequence, getPredefinedAllowedAttributeCode(), getValue(),
            getSequence());
  }

}
