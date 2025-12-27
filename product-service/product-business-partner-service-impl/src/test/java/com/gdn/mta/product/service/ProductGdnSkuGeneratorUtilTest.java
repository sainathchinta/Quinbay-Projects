package com.gdn.mta.product.service;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.util.ProductGdnSkuGeneratorUtilBean;

public class ProductGdnSkuGeneratorUtilTest {

  @InjectMocks
  ProductGdnSkuGeneratorUtilBean util;

  private static final String BP_CODE = "BLI-00106";
  private static final Long COUNTER = 10L;
  private static final int TOTAL_DIGITS = 5;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testAppendWithSerial() {
    Assertions.assertNotNull(util.appendWithSerial(BP_CODE, COUNTER, TOTAL_DIGITS));
  }
}
