package com.gdn.partners.pcu.external.validation.validator.Validations;


import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import jakarta.validation.ConstraintValidatorContext;

import static org.junit.jupiter.api.Assertions.assertFalse;


public class CreateBrandWipValidationTest {
   private static final String BRAND_NAME = "brandName";
   private static final String SPECIAL_BRAND_NAME = "™brandName";
   private static final String DESCRIPTION = "desc";
   private CreateBrandWipRequest createBrandWipRequest;

  @Mock
  private ConstraintValidatorContext constraintValidatorContext;

  @Mock
  private ConstraintValidatorContext.ConstraintViolationBuilder constraintViolationBuilder;

  private CreateBrandWipValidation createBrandWipValidation = new CreateBrandWipValidation();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setBrandName(BRAND_NAME);
    createBrandWipRequest.setBrandDescription(DESCRIPTION);
  }

  @Test
  public void test() {
    createBrandWipRequest.setBrandName(SPECIAL_BRAND_NAME);
    Mockito.doNothing().when(constraintValidatorContext).disableDefaultConstraintViolation();
    Mockito.doReturn(constraintViolationBuilder).when(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.CREATE_BRAND_INVALID_CHARACTER_ERR_MESSAGE);
    Mockito.when(constraintViolationBuilder.addConstraintViolation()).thenReturn(null);
    assertFalse(createBrandWipValidation.isValid(createBrandWipRequest, constraintValidatorContext));
    Mockito.verify(constraintValidatorContext).disableDefaultConstraintViolation();
    Mockito.verify(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.CREATE_BRAND_INVALID_CHARACTER_ERR_MESSAGE);
    Mockito.verify(constraintViolationBuilder).addConstraintViolation();
  }

  @Test
  public void isValidTest() {
    boolean response = createBrandWipValidation.isValid(createBrandWipRequest, constraintValidatorContext);
    Assertions.assertTrue(response);
  }

  @Test
  public void isValidTestEmptyDescription() {
    createBrandWipRequest.setBrandDescription(StringUtils.EMPTY);
    boolean response = createBrandWipValidation.isValid(createBrandWipRequest, constraintValidatorContext);
    assertFalse(response);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(constraintValidatorContext, constraintViolationBuilder);
  }
}