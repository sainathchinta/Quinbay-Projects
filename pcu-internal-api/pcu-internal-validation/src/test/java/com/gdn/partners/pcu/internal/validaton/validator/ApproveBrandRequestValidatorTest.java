package com.gdn.partners.pcu.internal.validaton.validator;

import jakarta.validation.ConstraintValidatorContext;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import com.gdn.partners.pcu.internal.web.model.request.ApproveBrandWipWebRequest;

public class ApproveBrandRequestValidatorTest {

  private static final String BRAND_REQUEST_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_DESCRIPTION = "brandDescription";
  private static final String INVALID_HTML_TAG = "<script>alert('asdasd');</script>";
  private static final String INVALID_BRAND_NAME = "ARE$@$@ ADå";

  private ApproveBrandWipWebRequest approveBrandWipWebRequest;

  private ApproveBrandRequestValidator approveBrandRequestValidator = new ApproveBrandRequestValidator();

  @Mock
  private ConstraintValidatorContext constraintValidatorContext;

  @BeforeEach
  public void setUp() throws Exception {
    approveBrandWipWebRequest = new ApproveBrandWipWebRequest();
    approveBrandWipWebRequest.setBrandRequestCode(BRAND_REQUEST_CODE);
    approveBrandWipWebRequest.setBrandName(BRAND_NAME);
    approveBrandWipWebRequest.setBrandDescription(BRAND_DESCRIPTION);
  }

  @Test
  public void isValidTest() {
    boolean response = approveBrandRequestValidator.isValid(approveBrandWipWebRequest, constraintValidatorContext);
    Assertions.assertTrue(response);
  }


  @Test
  public void isValidEmptyBrandRequestCodeTest() {
    approveBrandWipWebRequest.setBrandRequestCode(StringUtils.EMPTY);
    boolean response = approveBrandRequestValidator.isValid(approveBrandWipWebRequest, constraintValidatorContext);
    Assertions.assertFalse(response);
  }

  @Test
  public void isValidEmptyDescriptionTest() {
    approveBrandWipWebRequest.setBrandDescription(StringUtils.EMPTY);
    boolean response = approveBrandRequestValidator.isValid(approveBrandWipWebRequest, constraintValidatorContext);
    Assertions.assertFalse(response);
  }

  @Test
  public void isValidBrandNameInValidTest() {
    approveBrandWipWebRequest.setBrandName(INVALID_HTML_TAG);
    boolean response = approveBrandRequestValidator.isValid(approveBrandWipWebRequest, constraintValidatorContext);
    Assertions.assertFalse(response);
  }

  @Test
  public void isValidDescriptionInValidTest() {
    approveBrandWipWebRequest.setBrandDescription(INVALID_HTML_TAG);
    boolean response = approveBrandRequestValidator.isValid(approveBrandWipWebRequest, constraintValidatorContext);
    Assertions.assertFalse(response);
  }

  @Test
  public void isValidEmptyBrandNameTest() {
    approveBrandWipWebRequest.setBrandName(StringUtils.EMPTY);
    boolean response = approveBrandRequestValidator.isValid(approveBrandWipWebRequest, constraintValidatorContext);
    Assertions.assertFalse(response);
  }

  @Test
  public void isValidBrandNameTest() {
    approveBrandWipWebRequest.setBrandName(INVALID_BRAND_NAME);
    boolean response = approveBrandRequestValidator.isValid(approveBrandWipWebRequest, constraintValidatorContext);
    Assertions.assertFalse(response);
  }
}