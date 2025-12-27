package com.gdn.partners.pdt.service.configuration.distribution;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pdt.entity.configuration.distribution.AutoDistributionConfiguration;
import com.gdn.partners.pdt.model.configuration.distribution.PriorityType;
import com.gdn.partners.pdt.repository.configuration.distribution.AutoDistributionConfigurationRepository;

public class AutoDistributionConfigurationServiceTest {

  private static final String DEFAULT_PRIORITY_VALUE_1 = "PRIORITY VALUE 1";
  private static final String DEFAULT_PRIORITY_VALUE_2 = "PRIORITY VALUE 2";
  private static final String DEFAULT_PRIORITY_VALUE_3 = "PRIORITY VALUE 3";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  @Mock
  private AutoDistributionConfigurationRepository autoDistributionConfigurationRepository;

  @InjectMocks
  private AutoDistributionConfigurationServiceBean autoDistributionConfigurationServiceBean;

  private AutoDistributionConfiguration generateAutoDistributionConfiguration(PriorityType priorityType,
      String priorityValue) throws Exception {
    AutoDistributionConfiguration autoDistributionConfiguration = new AutoDistributionConfiguration();
    autoDistributionConfiguration.setPriorityType(priorityType);
    autoDistributionConfiguration.setPriorityValue(priorityValue);
    return autoDistributionConfiguration;
  }

  private List<AutoDistributionConfiguration> generateAutoDistributionConfigurations() throws Exception {
    List<AutoDistributionConfiguration> autoDistributionConfigurations = new ArrayList<AutoDistributionConfiguration>();
    autoDistributionConfigurations.add(this.generateAutoDistributionConfiguration(PriorityType.INITIATOR,
        AutoDistributionConfigurationServiceTest.DEFAULT_PRIORITY_VALUE_1));
    autoDistributionConfigurations.add(this.generateAutoDistributionConfiguration(PriorityType.INITIATOR,
        AutoDistributionConfigurationServiceTest.DEFAULT_PRIORITY_VALUE_2));
    return autoDistributionConfigurations;
  }

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.openMocks(this);
    List<AutoDistributionConfiguration> autoDistributionConfigurations = this.generateAutoDistributionConfigurations();
    Mockito.when(this.autoDistributionConfigurationRepository.saveAll(Mockito.anyCollection())).thenReturn(null);
    Mockito.when(
        this.autoDistributionConfigurationRepository.findByStoreIdAndVendorCodeAndMarkForDeleteIsFalse(
            Mockito.any(), Mockito.any())).thenReturn(autoDistributionConfigurations);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.autoDistributionConfigurationRepository);
  }

  @SuppressWarnings("unchecked")
  @Test
   void createTest() throws Exception {
    this.autoDistributionConfigurationServiceBean.create(null, this.generateAutoDistributionConfigurations());
    Mockito.verify(this.autoDistributionConfigurationRepository).saveAll(Mockito.anyCollection());
  }

  @SuppressWarnings("unchecked")
  @Test
   void updateTest() throws Exception {
    List<AutoDistributionConfiguration> autoDistributionConfigurations = new ArrayList<AutoDistributionConfiguration>();
    autoDistributionConfigurations.add(this.generateAutoDistributionConfiguration(PriorityType.INITIATOR,
        AutoDistributionConfigurationServiceTest.DEFAULT_PRIORITY_VALUE_1));
    autoDistributionConfigurations.add(this.generateAutoDistributionConfiguration(PriorityType.INITIATOR,
        AutoDistributionConfigurationServiceTest.DEFAULT_PRIORITY_VALUE_3));
    autoDistributionConfigurations.add(this.generateAutoDistributionConfiguration(PriorityType.BUSINESS_PARTNER_CODE,
        AutoDistributionConfigurationServiceTest.DEFAULT_PRIORITY_VALUE_3));
    this.autoDistributionConfigurationServiceBean.update(null, autoDistributionConfigurations);
    Mockito.verify(this.autoDistributionConfigurationRepository).findByStoreIdAndVendorCodeAndMarkForDeleteIsFalse(
        Mockito.any(), Mockito.any());
    Mockito.verify(this.autoDistributionConfigurationRepository).saveAll(Mockito.anyCollection());
  }

  @SuppressWarnings("unchecked")
  @Test
   void updateWithInvalidVendorCodeExceptionTest() throws Exception {
    Mockito.when(
      this.autoDistributionConfigurationRepository.findByStoreIdAndVendorCodeAndMarkForDeleteIsFalse(
        Mockito.any(), Mockito.any())).thenReturn(new ArrayList<AutoDistributionConfiguration>());
    Assertions.assertThrows(Exception.class, () -> {
      this.autoDistributionConfigurationServiceBean.update(null,
        this.generateAutoDistributionConfigurations());
    });
    Mockito.verify(this.autoDistributionConfigurationRepository)
      .findByStoreIdAndVendorCodeAndMarkForDeleteIsFalse(Mockito.any(), Mockito.any());
    Mockito.verify(this.autoDistributionConfigurationRepository,
      AutoDistributionConfigurationServiceTest.NEVER_CALLED).saveAll(Mockito.anyCollection());
  }

  @Test
   void findByVendorCodeTest() throws Exception {
    this.autoDistributionConfigurationServiceBean.findByVendorCode(null);
    Mockito.verify(this.autoDistributionConfigurationRepository).findByStoreIdAndVendorCodeAndMarkForDeleteIsFalse(
        Mockito.any(), Mockito.any());
  }

}
