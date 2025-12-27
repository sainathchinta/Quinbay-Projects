package com.gdn.x.productcategorybase.dto.allowedvalue;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

/**
 * Object response that hold allowed value for both predefined and defining attribute
 * 
 * @author agie.falah
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueResponse extends BaseResponse {
	
  private static final long serialVersionUID = 6887552025780755038L;
  private String attributeCode;
  private String attributeType;
  private List<AllowedValueResponse> allowedValue = new ArrayList<>();
  public AllowedAttributeValueResponse() {
    super();
  }
  public String getAttributeCode() {
    return attributeCode;
  }
  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }
  public String getAttributeType() {
    return attributeType;
  }
  public void setAttributeType(String attributeType) {
    this.attributeType = attributeType;
  }
  public List<AllowedValueResponse> getAllowedValue() {
    return allowedValue;
  }
  public void setAllowedValue(List<AllowedValueResponse> allowedValue) {
    this.allowedValue = allowedValue;
  }
  
}
