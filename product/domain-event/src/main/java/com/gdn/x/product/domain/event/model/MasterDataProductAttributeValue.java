package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.domain.event.enums.DescriptiveAttributeValueType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataProductAttributeValue implements Serializable {

  private static final long serialVersionUID = 8607493786806915701L;

  private String descriptiveAttributeValue;

  private DescriptiveAttributeValueType descriptiveAttributeValueType;

  private String allowedAttributeValueCode;

  private String predefinedAllowedAttributeValueCode;

  private MasterDataAllowedAttributeValue allowedAttributeValue;

  public MasterDataProductAttributeValue() {

  }

  public MasterDataProductAttributeValue(String descriptiveAttributeValue,
      DescriptiveAttributeValueType descriptiveAttributeValueType,
      String allowedAttributeValueCode, String predefinedAllowedAttributeValueCode) {
    super();
    this.descriptiveAttributeValue = descriptiveAttributeValue;
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
    this.allowedAttributeValueCode = allowedAttributeValueCode;
    this.predefinedAllowedAttributeValueCode = predefinedAllowedAttributeValueCode;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public MasterDataAllowedAttributeValue getAllowedAttributeValue() {
    return allowedAttributeValue;
  }

  public String getAllowedAttributeValueCode() {
    return this.allowedAttributeValueCode;
  }

  public String getDescriptiveAttributeValue() {
    return this.descriptiveAttributeValue;
  }

  public DescriptiveAttributeValueType getDescriptiveAttributeValueType() {
    return this.descriptiveAttributeValueType;
  }

  public String getPredefinedAllowedAttributeValueCode() {
    return this.predefinedAllowedAttributeValueCode;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAllowedAttributeValue(MasterDataAllowedAttributeValue allowedAttributeValue) {
    this.allowedAttributeValue = allowedAttributeValue;
  }

  public void setAllowedAttributeValueCode(String allowedAttributeValueCode) {
    this.allowedAttributeValueCode = allowedAttributeValueCode;
  }

  public void setDescriptiveAttributeValue(String descriptiveAttributeValue) {
    this.descriptiveAttributeValue = descriptiveAttributeValue;
  }

  public void setDescriptiveAttributeValueType(
      DescriptiveAttributeValueType descriptiveAttributeValueType) {
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
  }

  public void setPredefinedAllowedAttributeValueCode(String predefinedAllowedAttributeValueCode) {
    this.predefinedAllowedAttributeValueCode = predefinedAllowedAttributeValueCode;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataProductAttributeValue [descriptiveAttributeValue=%s, descriptiveAttributeValueType=%s, allowedAttributeValueCode=%s, predefinedAllowedAttributeValueCode=%s, allowedAttributeValue=%s, toString()=%s]",
            descriptiveAttributeValue, descriptiveAttributeValueType, allowedAttributeValueCode,
            predefinedAllowedAttributeValueCode, allowedAttributeValue, super.toString());
  }
}
