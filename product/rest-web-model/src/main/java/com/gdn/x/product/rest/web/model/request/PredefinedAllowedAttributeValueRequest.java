package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;



@JsonIgnoreProperties(ignoreUnknown = true)
public class PredefinedAllowedAttributeValueRequest extends BaseRequest {
  private static final long serialVersionUID = -1692041751922572253L;

  private String predefinedAllowedAttributeCode;
  private String value;
  private Integer sequence;

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

  public Integer getSequence() {
    return sequence;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }


}
