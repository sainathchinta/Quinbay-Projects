package com.gdn.mta.bulk.config;

import com.google.cloud.storage.Storage;
import com.google.cloud.storage.Bucket;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import static org.mockito.Mockito.when;
import org.mockito.ArgumentMatchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.test.util.ReflectionTestUtils;
import org.mockito.Mockito;

public class BeanConfigurationTest {

  private static final int DEFAULT_TIMEOUT_INTEGER = 10;
  private static final String DEFAULT_PROJECT_ID = "project-id";

  @InjectMocks
  private BeanConfiguration beanConfiguration;

  @Mock
  private Storage mockStorage;
  @Mock
  private Bucket mockBucket;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    beanConfiguration = new BeanConfiguration();
    beanConfiguration.setGcsProjectId(DEFAULT_PROJECT_ID);
    beanConfiguration.setGcsBulkBucketName(DEFAULT_PROJECT_ID);
    when(mockStorage.get(ArgumentMatchers.anyString(), ArgumentMatchers.any())).thenReturn(mockBucket);
    ReflectionTestUtils.setField(beanConfiguration, "gcsTimeout", DEFAULT_TIMEOUT_INTEGER);
    BeanConfiguration spyConfig = Mockito.spy(beanConfiguration);
    Mockito.doReturn(mockStorage).when(spyConfig).googleCloudStorage();
    this.beanConfiguration = spyConfig;
  }

  public void setBeanConfiguration(BeanConfiguration beanConfiguration) {
    this.beanConfiguration = beanConfiguration;
  }

  @Test
  public void testGetter() throws Exception {
    beanConfiguration.googleCloudStorage();
    beanConfiguration.bulkBucket();
    beanConfiguration.pricingBulkBucket();
    beanConfiguration.sourceImageBucket();
    beanConfiguration.finalImageBucket();
    BeanConfiguration.youTube();
  }
}