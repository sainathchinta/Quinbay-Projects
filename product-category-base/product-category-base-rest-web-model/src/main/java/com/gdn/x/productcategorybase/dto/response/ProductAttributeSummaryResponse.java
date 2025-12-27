package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeSummaryResponse extends BaseResponse {
  private static final long serialVersionUID = -1296872720699087892L;
  private String attributeType;
  private String attributeCode;
  private String attributeName;
  private String attributeValue;
  public ProductAttributeSummaryResponse() {
    super();
  }
  public String getAttributeType() {
    return attributeType;
  }
  public void setAttributeType(String attributeType) {
    this.attributeType = attributeType;
  }
  public String getAttributeCode() {
    return attributeCode;
  }
  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }
  public String getAttributeName() {
    return attributeName;
  }
  public void setAttributeName(String attributeName) {
    this.attributeName = attributeName;
  }
  public String getAttributeValue() {
    return attributeValue;
  }
  public void setAttributeValue(String attributeValue) {
    this.attributeValue = attributeValue;
  }
}
