package com.gdn.partners.pcu.internal.validaton.validator;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.springframework.util.StringUtils;

import com.gdn.partners.pcu.internal.validaton.annotation.ApproveBrandRequestValid;
import com.gdn.partners.pcu.internal.web.model.request.ApproveBrandWipWebRequest;

public class ApproveBrandRequestValidator
    implements ConstraintValidator<ApproveBrandRequestValid, ApproveBrandWipWebRequest> {

  private static final String HTML_TAG_PATTERN = "<(\"[^\"]*\"|'[^']*'|[^'\">])*>";
  private static Pattern pattern = Pattern.compile(HTML_TAG_PATTERN);
  private static final String VALID_CHARACTERS_REGEX = "[\\x20-\\x7E]+";

  @Override
  public void initialize(ApproveBrandRequestValid approveBrandRequestValid) {

  }

  @Override
  public boolean isValid(ApproveBrandWipWebRequest approveBrandWipWebRequest,
      ConstraintValidatorContext constraintValidatorContext) {
    return validateUpdate(approveBrandWipWebRequest);
  }

  private boolean validateUpdate(ApproveBrandWipWebRequest request) {
    if (StringUtils.isEmpty(request.getBrandName())) {
      return false;
    }
    if (StringUtils.isEmpty(request.getBrandRequestCode())) {
      return false;
    }
    if (StringUtils.isEmpty(request.getBrandDescription())) {
      return false;
    }
    if (validateHtmlTag(request.getBrandName())) {
      return false;
    }
    if (validateHtmlTag(request.getBrandDescription())) {
      return false;
    }
    if (!request.getBrandName().matches(VALID_CHARACTERS_REGEX)) {
      return false;
    }
    return true;
  }

  private static boolean validateHtmlTag(String input) {
    Matcher matcher = pattern.matcher(input);
    if (matcher.find()) {
      return true;
    } else {
      return false;
    }
  }
}
