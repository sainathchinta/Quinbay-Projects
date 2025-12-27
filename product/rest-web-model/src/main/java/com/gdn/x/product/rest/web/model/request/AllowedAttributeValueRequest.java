package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;


@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueRequest extends BaseRequest {
  private static final long serialVersionUID = 6408337293293270333L;
  
  private String allowedAttributeCode;
  private String value;
  private Integer sequence;

  public String getAllowedAttributeCode() {
    return allowedAttributeCode;
  }

  public void setAllowedAttributeCode(String allowedAttributeCode) {
    this.allowedAttributeCode = allowedAttributeCode;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public Integer getSequence() {
    return sequence;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }
}
