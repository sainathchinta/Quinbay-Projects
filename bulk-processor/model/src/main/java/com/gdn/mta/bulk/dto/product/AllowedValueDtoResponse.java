package com.gdn.mta.bulk.dto.product;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Object response that hold allowed attribute value for predefined and defining attribute from PCB
 * @author agie.falah
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedValueDtoResponse implements Serializable {
  private static final long serialVersionUID = -6081500440542899199L;
  private String allowedValueId;
  private String allowedValueCode;
  private String value;
  
  public AllowedValueDtoResponse() {
    super();
  }

  public AllowedValueDtoResponse(String allowedValueId, String allowedValueCode, String value) {
    super();
    this.allowedValueId = allowedValueId;
    this.allowedValueCode = allowedValueCode;
    this.value = value;
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
}
