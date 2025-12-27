package com.gdn.mta.product.util;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class BarCodeGeneratorTest {
  private static final int UPC_CODE_LENGTH = 17;
  private List<Integer> UPC_VALID_LENGTH = Arrays.asList(5,8,12,13,14,15);

  @Test
  public void generateUPCCodeTest() throws Exception {
    String upcCode = BarcodeGenerator.generateUPCCode();
    Assertions.assertEquals(UPC_CODE_LENGTH, upcCode.length());
  }

  @Test
  public void generateBan17Test() throws Exception {
    String barCode = BarcodeGenerator.generateBan17();
    Assertions.assertTrue(UPC_CODE_LENGTH == barCode.length() || UPC_CODE_LENGTH == barCode.length() - 1);
  }

  @Test
  public void isValidUPCCode_lengthTest() throws Exception {
    Assertions.assertTrue(BarcodeGenerator.isValidUPCCode("12345", UPC_VALID_LENGTH));
  }

  @Test
  public void isValidUPCCode_lengthTest_1() throws Exception {
    Assertions.assertTrue(BarcodeGenerator.isValidUPCCode("12345678", UPC_VALID_LENGTH));
  }

  @Test
  public void isValidUPCCode_lengthTest_2() throws Exception {
    Assertions.assertTrue(BarcodeGenerator.isValidUPCCode("123456789101", UPC_VALID_LENGTH));
  }

  @Test
  public void isValidUPCCode_ilengthTest_3() throws Exception {
    Assertions.assertTrue(BarcodeGenerator.isValidUPCCode("1234567891011", UPC_VALID_LENGTH));
  }

  @Test
  public void isValidUPCCodeTest_invalidTypeTest() throws Exception {
    Assertions.assertFalse(BarcodeGenerator.isValidUPCCode("String", UPC_VALID_LENGTH));
  }

  @Test
  public void isValidUPCCodeTest_2() throws Exception {
    Assertions.assertFalse(BarcodeGenerator.isValidUPCCode("123456", UPC_VALID_LENGTH));
    Assertions.assertTrue(BarcodeGenerator.isValidUPCCode("12345678901234", UPC_VALID_LENGTH));
  }

  @Test
  public void isValidUomUPCCodeTest() {
    Assertions.assertFalse(BarcodeGenerator.isValidUomUPCCode("A"));
    Assertions.assertTrue(BarcodeGenerator.isValidUomUPCCode("123"));
    StringBuilder stringBuilder = new StringBuilder();
    stringBuilder.append("1".repeat(255));
    Assertions.assertTrue(BarcodeGenerator.isValidUomUPCCode(stringBuilder.toString()));
    stringBuilder.append("1");
    Assertions.assertFalse(BarcodeGenerator.isValidUomUPCCode(stringBuilder.toString()));
  }

}
