package com.gdn.partners.pbp.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class Validator {

  public static final String EMAIL_PATTERN =
      "^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$";

  private Validator() {}

  public static boolean validate(ValidationType type, String value) throws Exception {
    boolean valid = false;
    if (ValidationType.EMAIL.equals(type)) {
      Pattern pattern = Pattern.compile(Validator.EMAIL_PATTERN);
      Matcher matcher = pattern.matcher(value);
      valid = matcher.matches();
    }
    return valid;
  }

}
