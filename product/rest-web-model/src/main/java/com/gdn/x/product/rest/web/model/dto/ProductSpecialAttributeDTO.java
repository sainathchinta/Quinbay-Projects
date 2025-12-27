package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSpecialAttributeDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private String attributeCode;
  private String attributeValue;
  private String attributeName;

  public ProductSpecialAttributeDTO() {

  }

  public ProductSpecialAttributeDTO(String attributeCode, String attributeValue) {
    this.attributeCode = attributeCode;
    this.attributeValue = attributeValue;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getAttributeCode() {
    return this.attributeCode;
  }

  public String getAttributeName() {
    return this.attributeName;
  }

  public String getAttributeValue() {
    return this.attributeValue;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }

  public void setAttributeName(String attributeName) {
    this.attributeName = attributeName;
  }

  public void setAttributeValue(String attributeValue) {
    this.attributeValue = attributeValue;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductSpecialAttributeDTO [attributeCode=%s, attributeValue=%s, attributeName=%s, toString()=%s]",
            this.attributeCode, this.attributeValue, this.attributeName, super.toString());
  }
}
