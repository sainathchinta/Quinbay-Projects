package com.gdn.x.mta.distributiontask.rest.model.request;

import com.gdn.x.mta.distributiontask.rest.model.base.DistributionBaseRequest;

/**
 * Created by virajjasani on 21/09/16.
 */
public class DistributionProductAttributeRequest extends DistributionBaseRequest {

  private static final long serialVersionUID = -5860742738916503163L;

  private String attributeCode;
  private String name;
  private String value;
  private String attributeType;
  private boolean variantCreation;

  public DistributionProductAttributeRequest() {
    // no implementation
  }

  public DistributionProductAttributeRequest(String attributeCode, String name,
      String value, String attributeType) {
    this.attributeCode = attributeCode;
    this.name = name;
    this.value = value;
    this.attributeType = attributeType;
  }

  public String getAttributeCode() {
    return attributeCode;
  }

  public void setAttributeCode(String attributeCode) {
    this.attributeCode = attributeCode;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public String getAttributeType() {
    return attributeType;
  }

  public void setAttributeType(String attributeType) {
    this.attributeType = attributeType;
  }

  public boolean isVariantCreation() {
    return variantCreation;
  }

  public void setVariantCreation(boolean variantCreation) {
    this.variantCreation = variantCreation;
  }

  @Override
  public String toString() {
    final StringBuilder sb =
        new StringBuilder("DistributionProductAttributeRequest{");
    sb.append("attributeCode='").append(attributeCode).append('\'');
    sb.append(", name='").append(name).append('\'');
    sb.append(", value='").append(value).append('\'');
    sb.append(", attributeType='").append(attributeType).append('\'');
    sb.append(", variantCreation='").append(variantCreation).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
