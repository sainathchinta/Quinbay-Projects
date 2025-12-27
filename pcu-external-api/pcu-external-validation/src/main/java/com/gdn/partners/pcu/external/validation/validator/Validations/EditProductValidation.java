package com.gdn.partners.pcu.external.validation.validator.Validations;

import java.util.Objects;
import java.util.regex.Pattern;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.validation.validator.Annotations.EditProductRequestValid;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoV2WebRequest;

public class EditProductValidation
    implements ConstraintValidator<EditProductRequestValid, ProductEditInfoV2WebRequest> {
  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  private static final String REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE = "([^\\x00-\\x7F])|(/ +/ig)";
  private static final Pattern PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE =
      Pattern.compile(REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE);
  private static final String REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
      "(\\<.*?\\>|&\\w+.;)|(/\\r\\n|\\n|\\r/gm)|([\\•|\\)]\\s+)";
  private static final Pattern PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
      Pattern.compile(REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT);
  private static final String REGEX_FOR_EXTRA_SPACE = "\\s+";
  private static final Pattern PATTERN_FOR_EXTRA_SPACE = Pattern.compile(REGEX_FOR_EXTRA_SPACE);

  @Override
  public void initialize(EditProductRequestValid editProductRequestValid) {

  }

  @Override
  public boolean isValid(ProductEditInfoV2WebRequest productEditInfoV2WebRequest,
      ConstraintValidatorContext constraintValidatorContext) {
    if (!(productEditInfoV2WebRequest.getProductSku().startsWith(mandatoryParameterHelper.getBusinessPartnerCode()))) {
      constraintValidatorContext.disableDefaultConstraintViolation();
      constraintValidatorContext.buildConstraintViolationWithTemplate(ErrorMessages.INVALID_GDN_SKU)
          .addConstraintViolation();
      return false;
    }

    if (Objects.nonNull(productEditInfoV2WebRequest.getUniqueSellingPoint())) {
      String uspWithoutTags = getFilterUSP(productEditInfoV2WebRequest.getUniqueSellingPoint());

      if (!(uspWithoutTags.length() <= Constants.MAXIMUM_UNIQUE_SELLING_POINT_LENGTH)) {
        constraintValidatorContext.disableDefaultConstraintViolation();
        constraintValidatorContext.buildConstraintViolationWithTemplate(
            ErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS).addConstraintViolation();
        return false;
      }
    }

    return true;
  }

  public static String getFilterUSP(String uniqueSellingPoint) {
    String usp = getUSPWithoutTags(PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE,
        getUSPWithoutTags(PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT, uniqueSellingPoint, StringUtils.EMPTY),
        StringUtils.EMPTY);
    return getUSPWithoutTags(PATTERN_FOR_EXTRA_SPACE, usp, StringUtils.SPACE);
  }

  private static String getUSPWithoutTags(Pattern pattern, String usp, String replace) {
    return pattern.matcher(usp).replaceAll(replace);
  }
}
