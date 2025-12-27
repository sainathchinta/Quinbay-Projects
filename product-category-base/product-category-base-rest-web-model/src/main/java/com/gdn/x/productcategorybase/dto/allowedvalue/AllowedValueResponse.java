package com.gdn.x.productcategorybase.dto.allowedvalue;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.ToString;

@ToString
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedValueResponse implements Serializable {

  private static final long serialVersionUID = -1440713193528480091L;
  private String allowedValueId;
  private String allowedValueCode;
  private String value;
  private String valueType;

  public AllowedValueResponse() {
    super();
  }

  public String getAllowedValueId() {
    return allowedValueId;
  }

  public void setAllowedValueId(String allowedValueId) {
    this.allowedValueId = allowedValueId;
  }

  public String getAllowedValueCode() {
    return allowedValueCode;
  }

  public void setAllowedValueCode(String allowedValueCode) {
    this.allowedValueCode = allowedValueCode;
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
}
