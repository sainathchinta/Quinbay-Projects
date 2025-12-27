package com.gdn.x.productcategorybase.dto.allowedvalue;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Object request to fetch specified allowed attribute value for predefined and defining attribute
 * 
 * @author agie.falah
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class AllowedAttributeValueRequest implements Serializable{

  private static final long serialVersionUID = -5360273115406273665L;
  private String attributeCode;
  private String attributeType;
  private List<String> values;
  public AllowedAttributeValueRequest() {
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
