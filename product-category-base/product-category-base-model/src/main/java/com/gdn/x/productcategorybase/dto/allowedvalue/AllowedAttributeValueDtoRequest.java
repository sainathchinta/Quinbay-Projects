package com.gdn.x.productcategorybase.dto.allowedvalue;

import java.io.Serializable;
import java.util.List;

/**
 * DTO request to fetch specified allowed attribute value for predefined and defining attribute
 * 
 * @author agie.falah
 *
 */
public class AllowedAttributeValueDtoRequest implements Serializable {

  private static final long serialVersionUID = 9204611294624763696L;
  private String attributeCode;
  private String attributeType;
  private List<String> values;
  public AllowedAttributeValueDtoRequest() {
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
  public List<String> getValues() {
    return values;
  }
  public void setValues(List<String> values) {
    this.values = values;
  }
  
}
