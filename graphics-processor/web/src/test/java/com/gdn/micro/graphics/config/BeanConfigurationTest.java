package com.gdn.micro.graphics.config;

import org.junit.jupiter.api.BeforeEach;
import org.mockito.InjectMocks;

public class BeanConfigurationTest {

  @InjectMocks
  private BeanConfiguration beanConfiguration;

  @BeforeEach
  public void setUp() throws Exception {
    beanConfiguration = new BeanConfiguration();
  }
}