package com.gdn.mta.product.service;

import org.junit.jupiter.api.Test;

import com.gdn.mta.product.util.BarcodeGenerator;
import com.gdn.mta.product.util.BaseGenerator;

public class GeneratorUtilTest {

  @SuppressWarnings("unused")
  @Test
  public void constructorTest() throws Exception {
    BaseGenerator baseGenerator = new BaseGenerator();
    BarcodeGenerator barcodeGenerator = new BarcodeGenerator();
  }

  @Test
  public void generateShippingWeightTest() throws Exception {
    BaseGenerator.generateShippingWeight(100.0, 100.0, 100.0, 1.0, 100);
  }
}
