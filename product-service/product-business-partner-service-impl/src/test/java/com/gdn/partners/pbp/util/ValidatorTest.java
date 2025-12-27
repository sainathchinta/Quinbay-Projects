package com.gdn.partners.pbp.util;

import org.junit.jupiter.api.Test;

public class ValidatorTest {

  private static final String DEFAULT_EMAIL = "email@mail.com";

  @Test
  public void validateWithEmailTest() throws Exception {
    Validator.validate(ValidationType.EMAIL, ValidatorTest.DEFAULT_EMAIL);
  }

  @Test
  public void validateWithUnregisteredValidationType() throws Exception {
    Validator.validate(null, null);
  }

}
