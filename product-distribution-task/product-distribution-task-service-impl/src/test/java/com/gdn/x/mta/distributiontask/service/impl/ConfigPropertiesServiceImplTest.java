package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.dao.api.ConfigPropertiesRepository;
import com.gdn.x.mta.distributiontask.model.ConfigProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ConfigPropertiesServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String CONFIG_PROPERTY_NAME = "propertyName";
  private static final String VALUE = "value";

  private final ConfigProperties configProperties = new ConfigProperties();

  @Mock
  private ConfigPropertiesRepository configPropertiesRepository;

  @InjectMocks
  private ConfigPropertiesServiceImpl configPropertiesServiceImpl;

  @BeforeEach
  public void init() {
    configProperties.setPropertyName(CONFIG_PROPERTY_NAME);
    configProperties.setValue(VALUE);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(configPropertiesRepository);
  }

  @Test
   void findByStoreIdAndPropertyNameAndMarkForDeleteFalseTest() {
    Mockito.when(this.configPropertiesRepository.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(
        STORE_ID, CONFIG_PROPERTY_NAME)).thenReturn(configProperties);
    ConfigProperties result =
        configPropertiesServiceImpl.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
            CONFIG_PROPERTY_NAME);
    Mockito.verify(this.configPropertiesRepository)
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID, CONFIG_PROPERTY_NAME);
    Assertions.assertEquals(CONFIG_PROPERTY_NAME, result.getPropertyName());
    Assertions.assertEquals(VALUE, result.getValue());
  }

  @Test
   void saveTest() {
    Mockito.when(this.configPropertiesServiceImpl.save(configProperties))
        .thenReturn(configProperties);
    ConfigProperties result = this.configPropertiesServiceImpl.save(configProperties);
    Mockito.verify(this.configPropertiesRepository).save(configProperties);
    Assertions.assertEquals(CONFIG_PROPERTY_NAME, result.getPropertyName());
    Assertions.assertEquals(VALUE, result.getValue());
  }
}
