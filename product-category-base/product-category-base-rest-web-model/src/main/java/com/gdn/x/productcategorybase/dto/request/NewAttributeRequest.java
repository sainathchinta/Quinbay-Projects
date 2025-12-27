package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class NewAttributeRequest extends BaseRequest {
  private static final long serialVersionUID = -6917566644879771639L;
  String attributeCode;
  String attributeValue;
  
  public NewAttributeRequest() {
    super();
  }
  public String getAttributeCode() {
    return attributeCode;
  }
  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }
  public String getAttributeValue() {
    return attributeValue;
  }
  public void setAttributeValue(String attributeValue) {
    this.attributeValue = attributeValue;
  }
}
