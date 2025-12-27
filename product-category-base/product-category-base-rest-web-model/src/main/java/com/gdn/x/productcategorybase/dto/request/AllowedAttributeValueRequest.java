package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueRequest extends BaseDTORequest {
  private static final long serialVersionUID = -1955258653680960878L;

  private String allowedAttributeCode;
  private String value;
  private String valueType;
  private Integer sequence;

  public AllowedAttributeValueRequest() {

  }

  public AllowedAttributeValueRequest(String value, Integer sequence, String storeId) {
    this.value = value;
    this.sequence = sequence;
    this.setStoreId(storeId);
  }

  public AllowedAttributeValueRequest(String value, String valueType, Integer sequence, String storeId) {
    this.value = value;
    this.valueType = valueType;
    this.sequence = sequence;
    this.setStoreId(storeId);
  }

  public String getAllowedAttributeCode() {
    return this.allowedAttributeCode;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public String getValue() {
    return this.value;
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

  public String getValueType() {
    return this.valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }


  @Override
  public String toString() {
    return String.format(
        "AllowedAttributeValue [allowedAttributeCode=%s, value=%s, valueType=%s, sequence=%s, "
            + "toString()=%s]",
        this.allowedAttributeCode, this.value, this.valueType, this.sequence, super.toString());
  }

}
