package com.gdn.partners.pcu.external.validation.validator.Validations;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoV2WebRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import jakarta.validation.ConstraintValidatorContext;

public class EditProductValidationTest {
  private static final String DESCRIPTION = "desc";
  private static final String BUSINESS_PARTNER_CODE = "BP-1234";
  private static final String USP_SELLING_POINT = "usp";
  private static final String PRODUCT_SKU = "productSku";
  private static final String VALID_PRODUCT_SKU = "BP-1234-1234";
  private static final String INVALID_USP =
      "This is a very long Unique Selling Point (USP) example that exceeds the 410-character limit set by the "
          + "validation constraints. It contains numerous words, sentences, and additional content to make it longer "
          + "than allowed. The purpose of this example is to demonstrate a USP that violates the length constraint."
          + " Please note that in practice, you should ensure that your application properly handles and validates "
          + "USPs to prevent exceeding maximum character limits.";


  @Mock
  private ConstraintValidatorContext constraintValidatorContext;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ConstraintValidatorContext.ConstraintViolationBuilder constraintViolationBuilder;

  @InjectMocks
  private EditProductValidation editProductValidation = new EditProductValidation();

  ProductEditInfoV2WebRequest productEditInfoV2WebRequest = new ProductEditInfoV2WebRequest();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productEditInfoV2WebRequest.setDescription(DESCRIPTION);
    productEditInfoV2WebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productEditInfoV2WebRequest.setProductSku(PRODUCT_SKU);

  }

  @Test
  public void getFilterUSP() {
  }

  @Test
  public void isValidTestWithInvalidProductSku() {
    Mockito.doNothing().when(constraintValidatorContext).disableDefaultConstraintViolation();
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    Mockito.doReturn(constraintViolationBuilder).when(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.INVALID_GDN_SKU);
    Mockito.when(constraintViolationBuilder.addConstraintViolation()).thenReturn(null);
    Assertions.assertFalse(editProductValidation.isValid(productEditInfoV2WebRequest, constraintValidatorContext));
    Mockito.verify(constraintValidatorContext).disableDefaultConstraintViolation();
    Mockito.verify(constraintValidatorContext).buildConstraintViolationWithTemplate(ErrorMessages.INVALID_GDN_SKU);
    Mockito.verify(constraintViolationBuilder).addConstraintViolation();
  }


  @Test
  public void isValidTestWithValidUSP() {
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    productEditInfoV2WebRequest.setProductSku(VALID_PRODUCT_SKU);
    productEditInfoV2WebRequest.setUniqueSellingPoint(USP_SELLING_POINT);
    Mockito.when(constraintViolationBuilder.addConstraintViolation()).thenReturn(null);
    Assertions.assertTrue(editProductValidation.isValid(productEditInfoV2WebRequest, constraintValidatorContext));
  }

  @Test
  public void isValidTestWithNullUSP() {
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    productEditInfoV2WebRequest.setProductSku(VALID_PRODUCT_SKU);
    Mockito.when(constraintViolationBuilder.addConstraintViolation()).thenReturn(null);
    Assertions.assertTrue(editProductValidation.isValid(productEditInfoV2WebRequest, constraintValidatorContext));
  }

  @Test
  public void isValidTestWithInvalidUSP() {
    Mockito.doNothing().when(constraintValidatorContext).disableDefaultConstraintViolation();
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_CODE);
    productEditInfoV2WebRequest.setProductSku(VALID_PRODUCT_SKU);
    productEditInfoV2WebRequest.setUniqueSellingPoint(INVALID_USP);
    Mockito.doReturn(constraintViolationBuilder).when(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    Mockito.when(constraintViolationBuilder.addConstraintViolation()).thenReturn(null);
    Assertions.assertFalse(editProductValidation.isValid(productEditInfoV2WebRequest, constraintValidatorContext));
    Mockito.verify(constraintValidatorContext).disableDefaultConstraintViolation();
    Mockito.verify(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    Mockito.verify(constraintViolationBuilder).addConstraintViolation();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(constraintValidatorContext, constraintViolationBuilder);
  }
}