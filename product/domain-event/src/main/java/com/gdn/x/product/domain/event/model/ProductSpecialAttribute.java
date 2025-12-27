package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSpecialAttribute implements Serializable {

  private static final long serialVersionUID = -3120105233327667087L;

  private String attributeCode;
  private String attributeName;
  private String attributeValue;

  public ProductSpecialAttribute() {

  }

  public ProductSpecialAttribute(String attributeCode, String attributeValue) {
    super();
    this.attributeCode = attributeCode;
    this.attributeValue = attributeValue;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
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
            "ProductSpecialAttribute [attributeCode=%s, attributeName=%s, attributeValue=%s, toString()=%s]",
            this.attributeCode, this.attributeName, this.attributeValue, super.toString());
  }

}
