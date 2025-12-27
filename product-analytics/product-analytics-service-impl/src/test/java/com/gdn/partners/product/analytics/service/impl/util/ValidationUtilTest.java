package com.gdn.partners.product.analytics.service.impl.util;

import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.hamcrest.MatcherAssert.assertThat;

@ExtendWith(MockitoExtension.class)
class ValidationUtilTest {

  @BeforeEach
  public void setup() {
  }

  @Test
  void isItemSkuTestNotValidFormat() {
    String sku = "HEH-123-123-123-123-123";
    boolean expectedResult = false;
    boolean result = ValidationUtil.isProductCode(sku);
    assertThat(result, CoreMatchers.equalTo(expectedResult));
  }

  @Test
  void isItemSkuTestValidFormat() {
    String sku = "HEH-123456";
    boolean expectedResult = true;
    boolean result = ValidationUtil.isProductCode(sku);
    assertThat(result, CoreMatchers.equalTo(expectedResult));
  }
}
