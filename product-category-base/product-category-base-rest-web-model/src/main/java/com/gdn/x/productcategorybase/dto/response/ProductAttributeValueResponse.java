package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeValueResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 3840276401678678659L;
  private AllowedAttributeValueResponse allowedAttributeValue;
  private String descriptiveAttributeValue;
  private DescriptiveAttributeValueType descriptiveAttributeValueType;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValue;

  public ProductAttributeValueResponse() {}

  public ProductAttributeValueResponse(AllowedAttributeValueResponse allowedAttributeValue,
      String descriptiveAttributeValue, DescriptiveAttributeValueType descriptiveAttributeValueType,
      PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValue, String storeId) {
    this.allowedAttributeValue = allowedAttributeValue;
    this.descriptiveAttributeValue = descriptiveAttributeValue;
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
    this.setStoreId(storeId);
  }

  public AllowedAttributeValueResponse getAllowedAttributeValue() {
    return this.allowedAttributeValue;
  }

  public String getDescriptiveAttributeValue() {
    return this.descriptiveAttributeValue;
  }

  public DescriptiveAttributeValueType getDescriptiveAttributeValueType() {
    return this.descriptiveAttributeValueType;
  }

  public PredefinedAllowedAttributeValueResponse getPredefinedAllowedAttributeValue() {
    return this.predefinedAllowedAttributeValue;
  }

  public void setAllowedAttributeValue(AllowedAttributeValueResponse allowedAttributeValue) {
    this.allowedAttributeValue = allowedAttributeValue;
  }

  public void setDescriptiveAttributeValue(String descriptiveAttributeValue) {
    this.descriptiveAttributeValue = descriptiveAttributeValue;
  }

  public void setDescriptiveAttributeValueType(DescriptiveAttributeValueType descriptiveAttributeValueType) {
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
  }

  public void setPredefinedAllowedAttributeValue(
      PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValue) {
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductAttributeValueResponse [allowedAttributeValue=%s, descriptiveAttributeValue=%s, descriptiveAttributeValueType=%s, predefinedAllowedAttributeValue=%s, toString()=%s]",
        this.allowedAttributeValue, this.descriptiveAttributeValue, this.descriptiveAttributeValueType,
        this.predefinedAllowedAttributeValue, super.toString());
  }
}
