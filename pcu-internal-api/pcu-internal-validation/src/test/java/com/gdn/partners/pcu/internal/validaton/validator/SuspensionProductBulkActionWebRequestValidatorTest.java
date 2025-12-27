package com.gdn.partners.pcu.internal.validaton.validator;

import java.util.Arrays;

import jakarta.validation.ConstraintValidatorContext;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SuspensionProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.UpdateBrandWebRequest;

public class SuspensionProductBulkActionWebRequestValidatorTest {

  private static final String REASON = "reason";
  private static final String NOTES = "notes";
  private static final String ACTION = "REACTIVATE";

  private SuspensionProductBulkActionWebRequestValidator suspensionProductBulkActionWebRequestValidator =
      new SuspensionProductBulkActionWebRequestValidator();
  private SuspensionProductBulkActionsWebRequest suspensionProductBulkActionsWebRequest;

  @Mock
  private ConstraintValidatorContext constraintValidatorContext;

  @Mock
  private ConstraintValidatorContext.ConstraintViolationBuilder constraintViolationBuilder;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    suspensionProductBulkActionsWebRequest = new SuspensionProductBulkActionsWebRequest();
    suspensionProductBulkActionsWebRequest.setNotes(NOTES);
    suspensionProductBulkActionsWebRequest.setReason(REASON);
    suspensionProductBulkActionsWebRequest.setAction(ACTION);
    suspensionProductBulkActionsWebRequest.setProducts(Arrays.asList(new ProductSuspensionWebRequest()));
  }

  @Test
  public void isValidTest() {
    boolean value = suspensionProductBulkActionWebRequestValidator
        .isValid(suspensionProductBulkActionsWebRequest, constraintValidatorContext);
    Assertions.assertTrue(value);
  }

  @Test
  public void isValidTest_withEmptyNotes() {
    Mockito.doReturn(constraintViolationBuilder).when(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.NOTES_TYPE_EMPTY);
    Mockito.when(constraintViolationBuilder.addConstraintViolation()).thenReturn(null);
    suspensionProductBulkActionsWebRequest.setAction(null);
    suspensionProductBulkActionsWebRequest.setNotes(null);
    boolean value = suspensionProductBulkActionWebRequestValidator
        .isValid(suspensionProductBulkActionsWebRequest, constraintValidatorContext);
    Assertions.assertFalse(value);
    Mockito.verify(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.NOTES_TYPE_EMPTY);
    Mockito.verify(constraintViolationBuilder).addConstraintViolation();
  }

  @Test
  public void isValidTest_withEmptyReason() {

    Mockito.doReturn(constraintViolationBuilder).when(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.REASON_TYPE_EMPTY);
    Mockito.when(constraintViolationBuilder.addConstraintViolation()).thenReturn(null);
    suspensionProductBulkActionsWebRequest.setAction(null);
    suspensionProductBulkActionsWebRequest.setReason(null);
    boolean value = suspensionProductBulkActionWebRequestValidator
        .isValid(suspensionProductBulkActionsWebRequest, constraintValidatorContext);
    Assertions.assertFalse(value);
    Mockito.verify(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.REASON_TYPE_EMPTY);
    Mockito.verify(constraintViolationBuilder).addConstraintViolation();
  }

  @Test
  public void isValidTest_withEmptyAction() {
    Mockito.doReturn(constraintViolationBuilder).when(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.ACTION_TYPE_INVALID);
    Mockito.when(constraintViolationBuilder.addConstraintViolation()).thenReturn(null);
    suspensionProductBulkActionsWebRequest.setAction(null);
    boolean value = suspensionProductBulkActionWebRequestValidator
        .isValid(suspensionProductBulkActionsWebRequest, constraintValidatorContext);
    Assertions.assertFalse(value);
    Mockito.verify(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.ACTION_TYPE_INVALID);
    Mockito.verify(constraintViolationBuilder).addConstraintViolation();
  }

  @Test
  public void isValidTest_withEmptyProductList() {
    Mockito.doReturn(constraintViolationBuilder).when(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.EMPTY_PRODUCT_LIST);
    Mockito.when(constraintViolationBuilder.addConstraintViolation()).thenReturn(null);
    suspensionProductBulkActionsWebRequest.setProducts(null);
    boolean value = suspensionProductBulkActionWebRequestValidator
        .isValid(suspensionProductBulkActionsWebRequest, constraintValidatorContext);
    Assertions.assertFalse(value);
    Mockito.verify(constraintValidatorContext)
        .buildConstraintViolationWithTemplate(ErrorMessages.EMPTY_PRODUCT_LIST);
    Mockito.verify(constraintViolationBuilder).addConstraintViolation();
  }
}