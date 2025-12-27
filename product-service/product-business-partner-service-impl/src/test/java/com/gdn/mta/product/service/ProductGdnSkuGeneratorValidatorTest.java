package com.gdn.mta.product.service;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.service.validator.ProductGdnSkuGeneratorValidatorBean;

public class ProductGdnSkuGeneratorValidatorTest {

  @InjectMocks
  private ProductGdnSkuGeneratorValidatorBean validator;

  private static final String BP_CODE = "BLI-00106";
  private ProductBusinessPartner defaultData;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    defaultData = new ProductBusinessPartner();
    defaultData.setBusinessPartnerId(BP_CODE);
  }
  
  @Test
  public void testValidateGenerateProductGdnSkuData() throws Exception {
    validator.validateGenerateProductGdnSkuData(defaultData);
  }

  @Test
  public void testValidateGenerateProductGdnSkuDataNull() throws Exception {
    Assertions.assertThrows(Exception.class, () -> {
      validator.validateGenerateProductGdnSkuData(null);
    });
  }

  @Test
  public void testValidateGenerateProductGdnSkuDataBpIdNull() throws Exception {
    defaultData.setBusinessPartnerId(null);
    Assertions.assertThrows(Exception.class, () -> {
      validator.validateGenerateProductGdnSkuData(defaultData);
    });
  }

  @Test
  public void testValidateGenerateProductGdnSkuDataBpIdBlank() throws Exception {
    defaultData.setBusinessPartnerId("");
    Assertions.assertThrows(Exception.class, () -> {
      validator.validateGenerateProductGdnSkuData(defaultData);
    });
  }

  @Test
  public void testValidateGenerateProductItemGdnSkuData() throws Exception {
    validator.validateGenerateProductItemGdnSkuData(1);
  }

  @Test
  public void testValidateGenerateProductItemGdnSkuDataInvalid() throws Exception {
    Assertions.assertThrows(Exception.class, () -> {
      validator.validateGenerateProductItemGdnSkuData(-1);
    });
  }
}
