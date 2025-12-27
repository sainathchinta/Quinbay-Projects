package com.gdn.mta.product.repository;

import org.junit.jupiter.api.Test;

import com.gdn.mta.product.config.ApplicationProperties;

public class ApplicationPropertiesTest {
  @Test
  public void testApplicationProperties() {
    ApplicationProperties appProperties = new ApplicationProperties();
    appProperties.setSkuDigits("5");
    appProperties.getSkuDigits();
  }
}
