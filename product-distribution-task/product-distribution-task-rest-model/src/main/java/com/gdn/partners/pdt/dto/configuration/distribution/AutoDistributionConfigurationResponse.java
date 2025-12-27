package com.gdn.partners.pdt.dto.configuration.distribution;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoDistributionConfigurationResponse extends BaseResponse {

  private static final long serialVersionUID = 1L;
  private String vendorCode;
  private String priorityType;
  private String priorityValue;

  public AutoDistributionConfigurationResponse() {}

  public AutoDistributionConfigurationResponse(String vendorCode, String priorityType, String priorityValue) {
    super();
    this.vendorCode = vendorCode;
    this.priorityType = priorityType;
    this.priorityValue = priorityValue;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public String getPriorityType() {
    return priorityType;
  }

  public void setPriorityType(String priorityType) {
    this.priorityType = priorityType;
  }

  public String getPriorityValue() {
    return priorityValue;
  }

  public void setPriorityValue(String priorityValue) {
    this.priorityValue = priorityValue;
  }

  @Override
  public String toString() {
    return String.format("AutoDistributionConfigurationResponse [vendorCode=%s, priorityType=%s, priorityValue=%s]",
        vendorCode, priorityType, priorityValue);
  }

}
