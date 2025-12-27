package com.gdn.partners.pcu.master.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.ToString;

@ToString
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueResponse extends BaseDTOResponse {
  private static final long serialVersionUID = 6408337293293270333L;

  private String allowedAttributeCode;
  private String value;
  private String valueType;
  private Integer sequence;

  public AllowedAttributeValueResponse() {

  }

  public AllowedAttributeValueResponse(String value, Integer sequence, String storeId) {
    this.value = value;
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
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }
}
