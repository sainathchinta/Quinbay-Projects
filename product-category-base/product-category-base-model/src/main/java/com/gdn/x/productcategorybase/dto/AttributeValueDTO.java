package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeValueDTO extends BaseResponse {

  private String predefinedAllowedAttributeCode;
  private String value;
  private String valueEn;
  private Integer sequence;
  private String allowedAttributeCode;
  private String valueType;

  public AttributeValueDTO() {
  }

  public AttributeValueDTO(String predefinedAllowedAttributeCode, String value, Integer sequence,
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

  public String getValueEn() {
    return this.valueEn;
  }

  public void setValueEn(String valueEn) {
    this.valueEn = valueEn;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("AttributeValueDTO{");
    sb.append("predefinedAllowedAttributeCode='").append(predefinedAllowedAttributeCode).append('\'');
    sb.append(", value='").append(value).append('\'');
    sb.append(", valueType='").append(value).append('\'');
    sb.append(", sequence=").append(sequence);
    sb.append(", allowedAttributeCode='").append(allowedAttributeCode).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
