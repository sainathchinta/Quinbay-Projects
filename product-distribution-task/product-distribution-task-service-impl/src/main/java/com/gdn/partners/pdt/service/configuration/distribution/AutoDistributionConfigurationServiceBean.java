package com.gdn.partners.pdt.service.configuration.distribution;


import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pdt.entity.configuration.distribution.AutoDistributionConfiguration;
import com.gdn.partners.pdt.repository.configuration.distribution.AutoDistributionConfigurationRepository;

@Service
@Transactional(readOnly = true)
public class AutoDistributionConfigurationServiceBean implements AutoDistributionConfigurationService{

  @Autowired
  private AutoDistributionConfigurationRepository autoDistributionConfigurationRepository;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void create(String vendorCode, List<AutoDistributionConfiguration> autoDistributionConfigurations)
      throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    for (AutoDistributionConfiguration autoDistributionConfiguration : autoDistributionConfigurations) {
      autoDistributionConfiguration.setStoreId(storeId);
      autoDistributionConfiguration.setVendorCode(vendorCode);
    }
    this.autoDistributionConfigurationRepository.saveAll(autoDistributionConfigurations);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(String vendorCode, List<AutoDistributionConfiguration> autoDistributionConfigurations)
      throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<AutoDistributionConfiguration> savedAutoDistributionConfigurations =
        this.autoDistributionConfigurationRepository.findByStoreIdAndVendorCodeAndMarkForDeleteIsFalse(storeId,
            vendorCode);
    if (CollectionUtils.isEmpty(savedAutoDistributionConfigurations)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "invalid vendor");
    }
    for (AutoDistributionConfiguration savedAutoDistributionConfiguration : savedAutoDistributionConfigurations) {
      boolean equals = false;
      for (AutoDistributionConfiguration autoDistributionConfiguration : autoDistributionConfigurations) {
        if (savedAutoDistributionConfiguration.getPriorityType()
            .equals(autoDistributionConfiguration.getPriorityType())
            && savedAutoDistributionConfiguration.getPriorityValue().equals(
                autoDistributionConfiguration.getPriorityValue())) {
          equals = true;
          break;
        }
      }
      if (!equals) {
        savedAutoDistributionConfiguration.setMarkForDelete(true);
      }
    }
    for (AutoDistributionConfiguration autoDistributionConfiguration : autoDistributionConfigurations) {
      boolean equals = false;
      for (AutoDistributionConfiguration savedAutoDistributionConfiguration : savedAutoDistributionConfigurations) {
        if (autoDistributionConfiguration.getPriorityType()
            .equals(savedAutoDistributionConfiguration.getPriorityType())
            && autoDistributionConfiguration.getPriorityValue().equals(
                savedAutoDistributionConfiguration.getPriorityValue())) {
          equals = true;
          break;
        }
      }
      if (!equals) {
        AutoDistributionConfiguration newAutoDistributionConfiguration = new AutoDistributionConfiguration();
        BeanUtils.copyProperties(autoDistributionConfiguration, newAutoDistributionConfiguration);
        newAutoDistributionConfiguration.setStoreId(storeId);
        newAutoDistributionConfiguration.setVendorCode(vendorCode);
        savedAutoDistributionConfigurations.add(newAutoDistributionConfiguration);
      }
    }
    this.autoDistributionConfigurationRepository.saveAll(savedAutoDistributionConfigurations);
  }

  @Override
  public List<AutoDistributionConfiguration> findByVendorCode(String vendorCode) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    return this.autoDistributionConfigurationRepository.findByStoreIdAndVendorCodeAndMarkForDeleteIsFalse(storeId,
        vendorCode);
  }

}
