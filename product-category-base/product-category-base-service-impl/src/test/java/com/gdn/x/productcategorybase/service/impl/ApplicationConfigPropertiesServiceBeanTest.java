package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.x.productcategorybase.entity.ApplicationConfigProperties;
import com.gdn.x.productcategorybase.repository.ApplicationConfigPropertiesRepository;

public class ApplicationConfigPropertiesServiceBeanTest {

  @Mock
  private ApplicationConfigPropertiesRepository applicationConfigPropertiesRepository;

  @InjectMocks
  private ApplicationConfigPropertiesServiceBean applicationConfigPropertiesServiceBean;

  private static final String STORE_ID = "10001";
  private static final String PROPERTY_NAME = "propertyName";

  @BeforeEach
  public void init() {
    initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(applicationConfigPropertiesRepository);
  }

  @Test
  public void findByStoreIdAndPropertyNameAndMarkForDeleteFalseTest() {
    applicationConfigPropertiesServiceBean.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID, PROPERTY_NAME);
    verify(applicationConfigPropertiesRepository)
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID, PROPERTY_NAME);
  }

  @Test
  public void saveTest() {
    ApplicationConfigProperties applicationConfigProperties = new ApplicationConfigProperties();
    applicationConfigPropertiesServiceBean.save(applicationConfigProperties);
    verify(applicationConfigPropertiesRepository).save(applicationConfigProperties);
  }
}