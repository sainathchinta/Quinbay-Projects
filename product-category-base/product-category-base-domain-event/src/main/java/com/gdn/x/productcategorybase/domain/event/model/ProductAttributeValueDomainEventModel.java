package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeValueDomainEventModel {

  private AllowedAttributeValueDomainEventModel allowedAttributeValue;
  private String descriptiveAttributeValue;
  private String descriptiveAttributeValueType;
  private PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttributeValue;

  public ProductAttributeValueDomainEventModel() {
    // do nothing
  }

  public ProductAttributeValueDomainEventModel(AllowedAttributeValueDomainEventModel allowedAttributeValue,
      String descriptiveAttributeValue, String descriptiveAttributeValueType,
      PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttributeValue) {
    super();
    this.allowedAttributeValue = allowedAttributeValue;
    this.descriptiveAttributeValue = descriptiveAttributeValue;
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
  }

  public AllowedAttributeValueDomainEventModel getAllowedAttributeValue() {
    return allowedAttributeValue;
  }

  public String getDescriptiveAttributeValue() {
    return descriptiveAttributeValue;
  }

  public String getDescriptiveAttributeValueType() {
    return descriptiveAttributeValueType;
  }

  public PredefinedAllowedAttributeValueDomainEventModel getPredefinedAllowedAttributeValue() {
    return predefinedAllowedAttributeValue;
  }

  public void setAllowedAttributeValue(AllowedAttributeValueDomainEventModel allowedAttributeValue) {
    this.allowedAttributeValue = allowedAttributeValue;
  }

  public void setDescriptiveAttributeValue(String descriptiveAttributeValue) {
    this.descriptiveAttributeValue = descriptiveAttributeValue;
  }

  public void setDescriptiveAttributeValueType(String descriptiveAttributeValueType) {
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
  }

  public void setPredefinedAllowedAttributeValue(
      PredefinedAllowedAttributeValueDomainEventModel predefinedAllowedAttributeValue) {
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductAttributeValueDomainEventModel [allowedAttributeValue=%s, descriptiveAttributeValue=%s, descriptiveAttributeValueType=%s, predefinedAllowedAttributeValue=%s, getAllowedAttributeValue()=%s, getDescriptiveAttributeValue()=%s, getDescriptiveAttributeValueType()=%s, getPredefinedAllowedAttributeValue()=%s]",
            allowedAttributeValue, descriptiveAttributeValue, descriptiveAttributeValueType,
            predefinedAllowedAttributeValue, getAllowedAttributeValue(), getDescriptiveAttributeValue(),
            getDescriptiveAttributeValueType(), getPredefinedAllowedAttributeValue());
  }

}
