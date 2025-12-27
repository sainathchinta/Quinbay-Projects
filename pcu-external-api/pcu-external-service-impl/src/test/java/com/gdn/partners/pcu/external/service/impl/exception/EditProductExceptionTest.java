package com.gdn.partners.pcu.external.service.impl.exception;

import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class EditProductExceptionTest {
  private static final String ERROR_CODE = "errorCode";
  private static final String ERROR_MESSAGE = "errorMessage";
  private EditProductWebResponse editProductWebResponse;

  private EditProductException editProductException;

  @BeforeEach
  public void setUp() {
    editProductException = new EditProductException(ERROR_CODE, ERROR_MESSAGE, editProductWebResponse);
  }

  @AfterEach
  public void tearDown() {
  }

  @Test
  public void EditProductExceptionTest() {
    try {
      throw new EditProductException(ERROR_CODE, ERROR_MESSAGE, editProductWebResponse);
    } catch (EditProductException e) {
      Assertions.assertNotNull(e);
      Assertions.assertEquals(ERROR_CODE, e.getErrorCode());
      Assertions.assertEquals(ERROR_MESSAGE, e.getErrorMessage());
      Assertions.assertEquals(editProductWebResponse, e.getEditProductWebResponse());
    }
  }
}