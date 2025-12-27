package com.gdn.partners.pdt.dto.configuration.distribution;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoDistributionConfigurationRequest implements Serializable {

  private static final long serialVersionUID = 1L;
  private String priorityType;
  private String priorityValue;

  public AutoDistributionConfigurationRequest() {}

  public AutoDistributionConfigurationRequest(String priorityType, String priorityValue) {
    super();
    this.priorityType = priorityType;
    this.priorityValue = priorityValue;
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
    return String.format("AutoDistributionConfigurationRequest [priorityType=%s, priorityValue=%s]", priorityType,
        priorityValue);
  }

}
