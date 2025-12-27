package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import lombok.AllArgsConstructor;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeDetailDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String attributeCode;
  private String attributeName;
  private String attributeValue;
  private String valueType;
  private String attributeValueEnglish;
  private String attributeType;

  public ProductAttributeDetailDTO() {}

  public ProductAttributeDetailDTO(String attributeName, String attributeValue) {
    this.attributeName = attributeName;
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

  public String getattributeValueEnglish() {
    return this.attributeValueEnglish;
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

  public void setattributeValueEnglish(String attributeValueEnglish) {
    this.attributeValueEnglish = attributeValueEnglish;
  }

  public String getAttributeType() {
    return attributeType;
  }

  public void setAttributeType(String attributeType) {
    this.attributeType = attributeType;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductAttributeDetailDTO [attributeCode=%s, attributeName=%s, attributeValue=%s, attributeValueEnglish=%s, attributeType=%s]",
        attributeCode, attributeName, attributeValue, attributeValueEnglish, attributeType);
  }

}
