package com.gdn.x.product.outbound.impl;


import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.x.product.outbound.config.CustomDeserializationProblemHandler;

public class CustomDeserializationProblemHandlerTest {

  private static final String PROPERTY = "PROPERTY";

  private CustomDeserializationProblemHandler customDeserializationProblemHandler;

  @BeforeEach
  public void init() {
    customDeserializationProblemHandler = new CustomDeserializationProblemHandler();
  }

  @Test
  public void handleUnknownPropertyTest() throws Exception {
    boolean result = customDeserializationProblemHandler.handleUnknownProperty(null, null, null, null, PROPERTY);
    Assertions.assertTrue(result);
  }
}
