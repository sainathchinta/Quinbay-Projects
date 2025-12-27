package com.gdn.x.productcategorybase.dto.allowedvalue;

import java.io.Serializable;

/**
 * DTO response to carry predefined and defining allowed value
 * 
 * @author agie.falah
 *
 */
public class AllowedValueDto implements Serializable {
  
  private static final long serialVersionUID = -539050993118673878L;
  private String allowedValueId;
  private String allowedValueCode;
  private String value;
  private String valueType;

  public AllowedValueDto() {
    super();
  }
  
  public AllowedValueDto(String allowedValueId, String allowedValueCode, String value) {
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

  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }
}
