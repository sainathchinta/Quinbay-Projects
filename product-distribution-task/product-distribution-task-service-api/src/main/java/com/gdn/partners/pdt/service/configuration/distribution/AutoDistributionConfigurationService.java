package com.gdn.partners.pdt.service.configuration.distribution;

import java.util.List;

import com.gdn.partners.pdt.entity.configuration.distribution.AutoDistributionConfiguration;

public interface AutoDistributionConfigurationService {

  void create(String vendorCode, List<AutoDistributionConfiguration> autoDistributionConfigurations) throws Exception;

  void update(String vendorCode, List<AutoDistributionConfiguration> autoDistributionConfigurations) throws Exception;

  List<AutoDistributionConfiguration> findByVendorCode(String vendorCode) throws Exception;

}
