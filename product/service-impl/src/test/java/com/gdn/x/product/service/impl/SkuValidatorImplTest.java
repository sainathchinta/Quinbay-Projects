package com.gdn.x.product.service.impl;


import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

/**
 * This unit test class template is the work of maria.o.a.sunaryo and felix.w.wijaya as
 * participation in Blibli Hackathon #1. Generated on 23 Jun 2016 22:41:12
 */
public class SkuValidatorImplTest {

  @InjectMocks
  private SkuValidatorImpl sut;

  @Test
  public void isItemSkuTestNotValidFormat() {
    String sku = "HEH-123-123-123-123-123";
    boolean expectedResult = false;
    boolean result = this.sut.isItemSku(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isItemSkuTestValidFormat() {
    String sku = "HEH-12345-12345-12345";
    boolean expectedResult = true;
    boolean result = this.sut.isItemSku(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isItemSkuTestWithBlankSku() {
    String sku = "";
    boolean expectedResult = false;
    boolean result = this.sut.isItemSku(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isProductSkuBlankSku() {
    String sku = "";
    boolean expectedResult = false;
    boolean result = this.sut.isProductSku(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isItemSkuTestWithNullSku() {
    boolean expectedResult = false;
    boolean result = this.sut.isItemSku(null);
    Assertions.assertEquals(result, false);
  }

  @Test
  public void isProductSkuTest() {
    String sku = "HEH-12345-12345";
    boolean expectedResult = true;
    boolean result = this.sut.isProductSku(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isProductSkuTestWithAlotOfDash() {
    String sku = "HEH-12-45-1-345";
    boolean expectedResult = false;
    boolean result = this.sut.isProductSku(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isProductSkuWrongFormat() {
    String sku = "HEH-12345-12345-123-123-123";
    boolean expectedResult = false;
    boolean result = this.sut.isProductSku(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isPristineIdTestAlphabetNotValidFormat() {
    String pristineId = "PRIS-001234-01";
    boolean expectedResult = false;
    boolean result = this.sut.isPristineId(pristineId);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isPristineIdTest6DigitNotValidFormat() {
    String pristineId = "PRI-00123-01";
    boolean expectedResult = false;
    boolean result = this.sut.isPristineId(pristineId);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isPristineIdTestLastDigitsDummyIdFormat() {
    String pristineId = "PRI-001234-03";
    boolean expectedResult = true;
    boolean result = this.sut.isPristineId(pristineId);
    Assertions.assertEquals(result, expectedResult);
  }


  @Test
  public void isPristineIdTestLastDigitsNotValidFormat() {
    String pristineId = "PRI-001234-04";
    boolean expectedResult = false;
    boolean result = this.sut.isPristineId(pristineId);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isPristineIdTestValidFormat() {
    String pristineId = "PRI-001234-01";
    boolean expectedResult = true;
    boolean result = this.sut.isPristineId(pristineId);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isPristineIdTestValidFormat1() {
    String pristineId = "PRI-001234-02";
    boolean expectedResult = true;
    boolean result = this.sut.isPristineId(pristineId);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isPristineIdTestWithBlankSku() {
    String pristineId = "";
    boolean expectedResult = false;
    boolean result = this.sut.isPristineId(pristineId);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isProductCodeTestValidFormat() {
    String productCode = "MTA-12345";
    boolean expectedResult = true;
    boolean result = this.sut.isProductCode(productCode);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isProductCodeTestWithBlankCode() {
    String productCode = "";
    boolean expectedResult = false;
    boolean result = this.sut.isProductCode(productCode);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isProductCodeTestInvalidFormat() {
    String productCode = "HEH-12345-12345-12345";
    boolean expectedResult = false;
    boolean result = this.sut.isProductCode(productCode);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isItemSkuL4OrL5TestNotValidFormat() {
    String sku = "HEH-123-123-123-123-123";
    boolean expectedResult = false;
    boolean result = this.sut.isItemSku(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isItemSkuL4OrL5TestValidFormatL4() {
    String sku = "HEH-12345-12345-12345";
    boolean expectedResult = true;
    boolean result = this.sut.isItemSkuL4OrL5(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isItemSkuL4OrL5TestValidFormatL5WithPrefixPP() {
    String sku = "HEH-12345-12345-12345-PP-3000001";
    boolean expectedResult = true;
    boolean result = this.sut.isItemSkuL4OrL5(sku);
    Assertions.assertEquals(result, expectedResult);
  }


  @Test
  public void isItemSkuL4OrL5TestValidFormatL5WithoutPrefixPP() {
    String sku = "HEH-12345-12345-12345-ABC-3000001";
    boolean expectedResult = true;
    boolean result = this.sut.isItemSkuL4OrL5(sku);
    Assertions.assertEquals(result, expectedResult);
  }

  @Test
  public void isItemSkuL4OrL5_WhenSkuL4OrL5NullTest() {
    boolean result = this.sut.isItemSkuL4OrL5(null);
    Assertions.assertEquals(result, false);
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {}
}
