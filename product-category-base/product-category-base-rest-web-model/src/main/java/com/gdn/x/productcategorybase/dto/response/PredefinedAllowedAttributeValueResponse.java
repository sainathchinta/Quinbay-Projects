package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
public class PredefinedAllowedAttributeValueResponse extends BaseDTOResponse {
  private static final long serialVersionUID = 5096384335468526230L;

  private String predefinedAllowedAttributeCode;
  private String value;
  private String valueEn;
  private Integer sequence;
  private String brandApprovalStatus = BrandWipState.APPROVED.name();
  private boolean protectedBrand;
  private String valueType;

  public PredefinedAllowedAttributeValueResponse() {}

  public PredefinedAllowedAttributeValueResponse(String value, Integer sequence, String storeId) {
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

  public void setPredefinedAllowedAttributeCode(String predefinedAllowedAttributeCode) {
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setValue(String value) {
    this.value = value;
  }

  public String getBrandApprovalStatus() {
    return brandApprovalStatus;
  }

  public void setBrandApprovalStatus(String brandApprovalStatus) {
    this.brandApprovalStatus = brandApprovalStatus;
  }

  public boolean isProtectedBrand() {
    return protectedBrand;
  }

  public void setProtectedBrand(boolean protectedBrand) {
    this.protectedBrand = protectedBrand;
  }

  public String getValueType() {
    return valueType;
  }

  public void setValueType(String valueType) {
    this.valueType = valueType;
  }

  public PredefinedAllowedAttributeValueResponse(String predefinedAllowedAttributeCode, String value, Integer sequence,
      String brandApprovalStatus, boolean protectedBrand) {
    this.predefinedAllowedAttributeCode = predefinedAllowedAttributeCode;
    this.value = value;
    this.sequence = sequence;
    this.brandApprovalStatus = brandApprovalStatus;
    this.protectedBrand = protectedBrand;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("PredefinedAllowedAttributeValueResponse{");
    sb.append("predefinedAllowedAttributeCode='").append(predefinedAllowedAttributeCode).append('\'');
    sb.append(", value='").append(value).append('\'');
    sb.append(", sequence=").append(sequence);
    sb.append(", brandApprovalStatus=").append(brandApprovalStatus);
    sb.append(", protectedBrand=").append(protectedBrand);
    sb.append(", valueEn='").append(valueEn).append('\'');
    sb.append('}');
    return sb.toString();
  }

  public String getValueEn() {
    return valueEn;
  }

  public void setValueEn(String valueEn) {
    this.valueEn = valueEn;
  }
}
