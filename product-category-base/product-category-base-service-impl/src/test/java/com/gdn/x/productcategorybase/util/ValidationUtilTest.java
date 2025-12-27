package com.gdn.x.productcategorybase.util;

import com.gdn.x.productcategorybase.exception.ValidationException;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

public class ValidationUtilTest {

  private static final String ERROR_CODE = "errorCode";
  private static final String ERROR_MESSAGE = "errorMessage";
  private static final String EXCEPTION_MESSAGE = "exceptionMessage";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String INTERNAL = "INTERNAL";

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testCheckParameter_ExpressionTrue() {
    try {
      ValidationUtil.checkParameter(true, ERROR_CODE, ERROR_MESSAGE);
    } catch (ValidationException e) {
      Assertions.fail(EXCEPTION_MESSAGE);
    }
  }

  @Test
  public void testCheckParameter_ExpressionFalse() {
    try {
      ValidationUtil.checkParameter(false, ERROR_CODE, ERROR_MESSAGE);
    } catch (ValidationException e) {
      Assertions.assertEquals(ERROR_CODE, e.getErrorCode());
      Assertions.assertEquals(ERROR_MESSAGE, e.getErrorMessage());
    }
  }

  @Test
  public void testCheckParameter2_ExpressionTrue() {
    try {
      ValidationUtil.checkParameter(true, ERROR_MESSAGE);
    } catch (ValidationException e) {
      Assertions.fail(EXCEPTION_MESSAGE);
    }
  }

  @Test
  public void testCheckParameter2_ExpressionFalse() {
    try {
      ValidationUtil.checkParameter(false, ERROR_MESSAGE);
    } catch (ValidationException e) {
      Assertions.assertEquals(ERROR_MESSAGE, e.getErrorCode());
      Assertions.assertEquals(ERROR_MESSAGE, e.getErrorMessage());
    }
  }

  @Test
  public void isExternalTrueTest() {
    Assertions.assertTrue(ValidationUtil.isExternal(BUSINESS_PARTNER_CODE));
  }
  @Test
  public void isExternalFalseTest() {
    Assertions.assertFalse(ValidationUtil.isExternal(INTERNAL));
  }
}
