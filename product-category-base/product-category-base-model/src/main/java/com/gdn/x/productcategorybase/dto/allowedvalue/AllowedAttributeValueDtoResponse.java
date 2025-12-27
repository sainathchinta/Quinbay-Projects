package com.gdn.x.productcategorybase.dto.allowedvalue;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * DTO request to fetch specified allowed attribute value for predefined and defining attribute
 * 
 * @author agie.falah
 *
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueDtoResponse implements Serializable {

  private static final long serialVersionUID = 8716568707188872757L;
  private String attributeCode;
  private String attributeType;
  private List<AllowedValueDto> allowedValue;
  public AllowedAttributeValueDtoResponse() {
    super();
  }
  
  public AllowedAttributeValueDtoResponse(String attributeCode, String attributeType,
      List<AllowedValueDto> allowedValue) {
    super();
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    this.allowedValue = allowedValue;
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
  public List<AllowedValueDto> getAllowedValue() {
    return allowedValue;
  }
  public void setAllowedValue(List<AllowedValueDto> allowedValue) {
    this.allowedValue = allowedValue;
  }
  @Override
  public String toString() {
	StringBuilder builder = new StringBuilder();
	builder.append("AllowedAttributeValueDtoResponse [attributeCode=").append(attributeCode).append(", attributeType=")
			.append(attributeType).append(", allowedValue=").append(allowedValue).append("]");
	return builder.toString();
  }
}
