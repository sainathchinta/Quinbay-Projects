package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PredefinedAllowedAttributeValueDTO {

  private String predefinedAllowedAttributeCode;

  private String value;

  private Integer sequence;

  public PredefinedAllowedAttributeValueDTO() {}

  public PredefinedAllowedAttributeValueDTO(String predefinedAllowedAttributeCode, String value,
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

  @Override
  public String toString() {
    return String
        .format(
            "PredefinedAllowedAttributeValue [predefinedAllowedAttributeCode=%s, value=%s, sequence=%s, toString()=%s]",
            predefinedAllowedAttributeCode, value, sequence, super.toString());
  }
}
