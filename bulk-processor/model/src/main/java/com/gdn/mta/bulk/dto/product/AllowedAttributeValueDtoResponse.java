package com.gdn.mta.bulk.dto.product;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

/**
 * Object response to hold list of allowed value for predefined and defining attribute from PCB
 * @author agie.falah
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueDtoResponse extends BaseResponse {
  private static final long serialVersionUID = -2422327666199891676L;
  private String attributeCode;
  private String attributeType;
  private List<AllowedValueDtoResponse> allowedValue;
  
  public AllowedAttributeValueDtoResponse() {
    super();
  }

  public AllowedAttributeValueDtoResponse(String attributeCode, String attributeType,
      List<AllowedValueDtoResponse> allowedValue) {
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

  public List<AllowedValueDtoResponse> getAllowedValue() {
    return allowedValue;
  }

  public void setAllowedValue(List<AllowedValueDtoResponse> allowedValue) {
    this.allowedValue = allowedValue;
  }
  
}
