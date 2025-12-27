package com.gdn.x.productcategorybase.controller;

import com.gdn.x.productcategorybase.exception.ValidationException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import static org.mockito.MockitoAnnotations.initMocks;

public class ErrorControllerTest {

  @InjectMocks
  ErrorController errorController;


  @BeforeEach
  public void setUp() {
    initMocks(this);
  }

  @Test
  public void validationExceptionTest() throws Exception {
    errorController.validationException(new ValidationException("ERROR_CODE","ERROR_MESSAGE"));
  }
}
