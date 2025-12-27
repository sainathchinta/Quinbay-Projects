package com.gdn.partners.pcu.internal.validaton.validator;

import jakarta.validation.ConstraintValidatorContext;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import com.gdn.partners.pcu.internal.web.model.request.UpdateBrandWebRequest;

public class UpdateBrandRequestValidatorTest {

  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_DESCRIPTION = "brandDescription";
  private UpdateBrandWebRequest updateBrandWebRequest;

  private UpdateBrandRequestValidator updateBrandRequestValidator = new UpdateBrandRequestValidator();

  @Mock
  private ConstraintValidatorContext constraintValidatorContext;

  @BeforeEach
  public void setUp() throws Exception {

    updateBrandWebRequest = new UpdateBrandWebRequest();
    updateBrandWebRequest.setBrandCode(BRAND_CODE);
    updateBrandWebRequest.setBrandName(BRAND_NAME);
    updateBrandWebRequest.setBrandDescription(BRAND_DESCRIPTION);
  }

  @Test
  public void isValidTest() {
    boolean value = updateBrandRequestValidator.isValid(updateBrandWebRequest, constraintValidatorContext);
    Assertions.assertTrue(value);
  }

  @Test
  public void isValidTest_withEmptyBrandCode() {
    updateBrandWebRequest.setBrandCode(null);
    boolean value = updateBrandRequestValidator.isValid(updateBrandWebRequest, constraintValidatorContext);
    Assertions.assertFalse(value);
  }

  @Test
  public void isValidTest_withEmptyBrandName() {
    updateBrandWebRequest.setBrandDescription(null);
    boolean value = updateBrandRequestValidator.isValid(updateBrandWebRequest, constraintValidatorContext);
    Assertions.assertFalse(value);
  }
}