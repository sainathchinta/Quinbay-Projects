package com.gdn.x.mta.distributiontask.rest.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

/**
 * Created by virajjasani on 20/09/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class DistributionProductAttributeResponse extends BaseResponse {

  private static final long serialVersionUID = -3877424990843063971L;

  private String attributeCode;
  private String name;
  private String value;
  private String attributeType;
  private boolean variantCreation;
  private boolean extractedValue;
  private boolean dsExtraction;

  public DistributionProductAttributeResponse() {
    // no implementation
  }

  public DistributionProductAttributeResponse(String attributeCode, String name,
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

  public boolean isExtractedValue() {
    return extractedValue;
  }

  public void setExtractedValue(boolean extractedValue) {
    this.extractedValue = extractedValue;
  }

  public boolean isDsExtraction() {
    return dsExtraction;
  }

  public void setDsExtraction(boolean dsExtraction) {
    this.dsExtraction = dsExtraction;
  }

  @Override
  public String toString() {
    final StringBuilder sb =
        new StringBuilder("DistributionProductAttributeResponse{");
    sb.append("attributeCode='").append(attributeCode).append('\'');
    sb.append(", name='").append(name).append('\'');
    sb.append(", value='").append(value).append('\'');
    sb.append(", attributeType='").append(attributeType).append('\'');
    sb.append(", variantCreation").append(variantCreation);
    sb.append(", extractedValue").append(extractedValue);
    sb.append(", dsExtraction").append(dsExtraction);
    sb.append('}');
    return sb.toString();
  }
}
