package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.AllArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
public class PredefinedAllowedAttributeValueRequest extends BaseDTORequest {
  private static final long serialVersionUID = -6227105884139804821L;

  private String predefinedAllowedAttributeCode;
  private String value;
  private String valueEn;
  private Integer sequence;

  public PredefinedAllowedAttributeValueRequest() {}

  public PredefinedAllowedAttributeValueRequest(String value, Integer sequence, String storeId) {
    this.value = value;
    this.sequence = sequence;
    this.setStoreId(storeId);
  }

  public String getPredefinedAllowedAttributeCode() {
    return this.predefinedAllowedAttributeCode;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public String getValue() {
    return this.value;
  }

  public String getValueEn() {
    return this.valueEn;
  }

  public void setPredefinedAllowedAttributeCode(String predefinedAllowedAttributeCode) {
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public void setValueEn(String valueEn) {
    this.valueEn = valueEn;
  }

  @Override
  public String toString() {
    return String.format(
        "PredefinedAllowedAttributeValueRequest [predefinedAllowedAttributeCode=%s, value=%s, "
            + "valueEn=%s, "
            + "sequence=%s, toString()=%s]",
        this.predefinedAllowedAttributeCode, this.value, this.valueEn, this.sequence, super.toString());
  }
}
