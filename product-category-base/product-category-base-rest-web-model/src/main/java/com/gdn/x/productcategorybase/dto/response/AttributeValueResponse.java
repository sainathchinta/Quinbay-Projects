package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeValueResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -2655577351780961875L;
  private String predefinedAllowedAttributeCode;
  private String value;
  private String valueEn;
  private Integer sequence;
  private String allowedAttributeCode;
  private String valueType;

  public AttributeValueResponse() {
  }

  public AttributeValueResponse(String predefinedAllowedAttributeCode, String value, Integer sequence,
      String allowedAttributeCode) {
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
    this.value = value;
    this.sequence = sequence;
    this.allowedAttributeCode = allowedAttributeCode;
  }

  public AttributeValueResponse(String predefinedAllowedAttributeCode, String value, Integer sequence,
      String allowedAttributeCode, String valueType) {
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
    this.value = value;
    this.sequence = sequence;
    this.allowedAttributeCode = allowedAttributeCode;
    this.valueType = valueType;
  }

  public String getPredefinedAllowedAttributeCode() {
    return predefinedAllowedAttributeCode;
  }

  public void setPredefinedAllowedAttributeCode(String predefinedAllowedAttributeCode) {
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public void setValueEn(String valueEn) {
    this.valueEn = valueEn;
  }

  public String getValueEn() {
    return this.valueEn;
  }

  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }

  public Integer getSequence() {
    return sequence;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public String getAllowedAttributeCode() {
    return allowedAttributeCode;
  }

  public void setAllowedAttributeCode(String allowedAttributeCode) {
    this.allowedAttributeCode = allowedAttributeCode;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("AttributeValueResponse{");
    sb.append("predefinedAllowedAttributeCode='")
        .append(predefinedAllowedAttributeCode)
        .append('\'');
    sb.append(", value='").append(value).append('\'');
    sb.append(", valueType='").append(valueType).append('\'');
    sb.append(", sequence=").append(sequence);
    sb.append(", allowedAttributeCode='").append(allowedAttributeCode).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
