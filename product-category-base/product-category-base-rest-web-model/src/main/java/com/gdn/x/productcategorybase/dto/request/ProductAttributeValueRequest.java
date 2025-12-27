package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeValueRequest extends BaseDTORequest {

  private static final long serialVersionUID = 3840276401678678659L;
  private AllowedAttributeValueRequest allowedAttributeValue;
  private String descriptiveAttributeValue;
  private DescriptiveAttributeValueType descriptiveAttributeValueType;
  private PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue;

  public ProductAttributeValueRequest() {}

  public ProductAttributeValueRequest(AllowedAttributeValueRequest allowedAttributeValue,
      String descriptiveAttributeValue, DescriptiveAttributeValueType descriptiveAttributeValueType, String storeId) {
    this.allowedAttributeValue = allowedAttributeValue;
    this.descriptiveAttributeValue = descriptiveAttributeValue;
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
    this.setStoreId(storeId);
  }

  public AllowedAttributeValueRequest getAllowedAttributeValue() {
    return this.allowedAttributeValue;
  }

  public String getDescriptiveAttributeValue() {
    return this.descriptiveAttributeValue;
  }

  public DescriptiveAttributeValueType getDescriptiveAttributeValueType() {
    return this.descriptiveAttributeValueType;
  }

  public PredefinedAllowedAttributeValueRequest getPredefinedAllowedAttributeValue() {
    return this.predefinedAllowedAttributeValue;
  }

  public void setAllowedAttributeValue(AllowedAttributeValueRequest allowedAttributeValue) {
    this.allowedAttributeValue = allowedAttributeValue;
  }

  public void setDescriptiveAttributeValue(String descriptiveAttributeValue) {
    this.descriptiveAttributeValue = descriptiveAttributeValue;
  }

  public void setDescriptiveAttributeValueType(DescriptiveAttributeValueType descriptiveAttributeValueType) {
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
  }

  public void setPredefinedAllowedAttributeValue(
      PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue) {
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductAttributeValueRequest [predefinedAllowedAttributeValue=%s, allowedAttributeValue=%s, descriptiveAttributeValue=%s, descriptiveAttributeValueType=%s, toString()=%s]",
        this.predefinedAllowedAttributeValue, this.allowedAttributeValue, this.descriptiveAttributeValue,
        this.descriptiveAttributeValueType, super.toString());
  }
}
