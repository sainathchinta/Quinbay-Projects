package com.gdn.partners.pdt.dto.configuration.distribution;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CreateAutoDistributionConfigurationRequest implements Serializable {

  private static final long serialVersionUID = 1L;
  private String vendorCode;
  private List<AutoDistributionConfigurationRequest> autoDistributionConfigurations =
      new ArrayList<AutoDistributionConfigurationRequest>();

  public CreateAutoDistributionConfigurationRequest() {}

  public CreateAutoDistributionConfigurationRequest(String vendorCode,
      List<AutoDistributionConfigurationRequest> autoDistributionConfigurations) {
    super();
    this.vendorCode = vendorCode;
    this.autoDistributionConfigurations = autoDistributionConfigurations;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public List<AutoDistributionConfigurationRequest> getAutoDistributionConfigurations() {
    return autoDistributionConfigurations;
  }

  public void setAutoDistributionConfigurations(
      List<AutoDistributionConfigurationRequest> autoDistributionConfigurations) {
    this.autoDistributionConfigurations = autoDistributionConfigurations;
  }

  @Override
  public String toString() {
    return String.format(
        "CreateAutoDistributionConfigurationRequest [vendorCode=%s, autoDistributionConfigurations=%s]", vendorCode,
        autoDistributionConfigurations);
  }

}
