package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeValueRequest extends BaseRequest {
  private static final long serialVersionUID = 3402834398976009392L;

  private AllowedAttributeValueRequest allowedAttributeValue;
  private String descriptiveAttributeValue;
  private DescriptiveAttributeValueType descriptiveAttributeValueType;
  private PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue;
  private boolean markForDelete;

  public AllowedAttributeValueRequest getAllowedAttributeValue() {
    return allowedAttributeValue;
  }

  public void setAllowedAttributeValue(AllowedAttributeValueRequest allowedAttributeValue) {
    this.allowedAttributeValue = allowedAttributeValue;
  }

  public String getDescriptiveAttributeValue() {
    return descriptiveAttributeValue;
  }

  public void setDescriptiveAttributeValue(String descriptiveAttributeValue) {
    this.descriptiveAttributeValue = descriptiveAttributeValue;
  }

  public DescriptiveAttributeValueType getDescriptiveAttributeValueType() {
    return descriptiveAttributeValueType;
  }

  public void setDescriptiveAttributeValueType(
      DescriptiveAttributeValueType descriptiveAttributeValueType) {
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
  }

  public PredefinedAllowedAttributeValueRequest getPredefinedAllowedAttributeValue() {
    return predefinedAllowedAttributeValue;
  }

  public void setPredefinedAllowedAttributeValue(
      PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue) {
    this.predefinedAllowedAttributeValue = predefinedAllowedAttributeValue;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }
}
