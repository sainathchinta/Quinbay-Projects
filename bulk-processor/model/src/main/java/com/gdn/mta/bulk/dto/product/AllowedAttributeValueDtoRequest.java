package com.gdn.mta.bulk.dto.product;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

/**
 * Object request to fetch specified allowed attribute value for predefined and defining attribute from PCB
 * 
 * @author agie.falah
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueDtoRequest extends BaseResponse {
  private static final long serialVersionUID = 7665990956821200409L;
  private String attributeCode;
  private String attributeType;
  private List<String> values;
  public AllowedAttributeValueDtoRequest() {
    super();
  }
  public AllowedAttributeValueDtoRequest(String attributeCode, String attributeType,
      List<String> values) {
    super();
    this.attributeCode = attributeCode;
    this.attributeType = attributeType;
    this.values = values;
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
  public List<String> getValues() {
    return values;
  }
  public void setValues(List<String> values) {
    this.values = values;
  }
  
}
