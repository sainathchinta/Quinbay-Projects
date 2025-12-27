package com.gdn.mta.bulk.helper;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;

public class ProductLevel3ProcessorValidationMessageHelperTest {

  private String ATTRIBUTE_NAME = "ATTRIBUTE_NAME";
  private String IMAGE_NAME = "img.jpg";

  @Test
  public void errorMessageTest() {
    String errorMessage = ProductLevel3ProcessorValidationMessageHelper
        .errorMessage(IMAGE_NAME, BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE,
            IMAGE_NAME);
    Assertions.assertNotNull(errorMessage);
    errorMessage.contains(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE);
  }

  @Test
  public void errorMessagePrependEmptyTest() {
    String errorMessage = ProductLevel3ProcessorValidationMessageHelper.errorMessage(StringUtils.EMPTY,
        BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE, IMAGE_NAME);
    Assertions.assertNotNull(errorMessage);
    errorMessage.contains(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE);
  }

  @Test
  public void errorMessageAppendEmptyTest() {
    String errorMessage = ProductLevel3ProcessorValidationMessageHelper
        .errorMessage(IMAGE_NAME, BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE,
            StringUtils.EMPTY);
    Assertions.assertNotNull(errorMessage);
    errorMessage.contains(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE);
  }

  @Test
  public void errorMessageEmptyTest() {
    String errorMessage = ProductLevel3ProcessorValidationMessageHelper.errorMessage(StringUtils.EMPTY,
        BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE, StringUtils.EMPTY);
    Assertions.assertNotNull(errorMessage);
    errorMessage.contains(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE);
  }

  @Test
  public void errorMessageBasedOnMerchantTest() {
    String result = ProductLevel3ProcessorValidationMessageHelper.errorMessageBasedOnMerchant(true,
        BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN,
        BulkProcessValidationErrorMessages.IMAGE_SIZE_VALIDATION_ERR_MESSAGE);
    Assertions.assertEquals(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN, result);
  }

  @Test
  public void errorMessageBasedOnMerchantFalseTest() {
    String result = ProductLevel3ProcessorValidationMessageHelper.errorMessageBasedOnMerchant(false,
        BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN,
        BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE);
    Assertions.assertEquals(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE, result);
  }
}