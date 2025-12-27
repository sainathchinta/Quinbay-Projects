package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemAttributeValueResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 8171200506837539231L;

  private AttributeResponse attributeResponse;

  private String value;

  private String valueType;

  public ProductItemAttributeValueResponse() {}

  public ProductItemAttributeValueResponse(AttributeResponse attributeResponse, String value) {
    this.attributeResponse = attributeResponse;
    this.value = value;
  }

  public AttributeResponse getAttributeResponse() {
    return this.attributeResponse;
  }

  public String getValue() {
    return this.value;
  }

  public void setAttributeResponse(AttributeResponse attributeResponse) {
    this.attributeResponse = attributeResponse;
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
