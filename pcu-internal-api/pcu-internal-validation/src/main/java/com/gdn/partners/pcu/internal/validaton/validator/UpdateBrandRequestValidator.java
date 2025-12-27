package com.gdn.partners.pcu.internal.validaton.validator;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.springframework.util.StringUtils;

import com.gdn.partners.pcu.internal.validaton.annotation.UpdateBrandRequestValid;
import com.gdn.partners.pcu.internal.web.model.request.UpdateBrandWebRequest;

public class UpdateBrandRequestValidator implements
    ConstraintValidator<UpdateBrandRequestValid, UpdateBrandWebRequest> {

  private static final String HTML_TAG_PATTERN = "<(\"[^\"]*\"|'[^']*'|[^'\">])*>";
  private static Pattern pattern = Pattern.compile(HTML_TAG_PATTERN);

  @Override
  public void initialize(UpdateBrandRequestValid updateBrandRequestValid) {

  }

  @Override
  public boolean isValid(UpdateBrandWebRequest updateBrandWebRequest,
      ConstraintValidatorContext constraintValidatorContext) {
    return validateUpdate(updateBrandWebRequest);
  }

  private boolean validateUpdate(UpdateBrandWebRequest request) {
    if (StringUtils.isEmpty(request.getBrandCode())) {
      return false;
    }
    if(StringUtils.isEmpty(request.getBrandDescription())) {
      return false;
    }
    if(validateHtmlTag(request.getBrandName())) {
      return false;
    }
    if(validateHtmlTag(request.getBrandDescription())) {
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
