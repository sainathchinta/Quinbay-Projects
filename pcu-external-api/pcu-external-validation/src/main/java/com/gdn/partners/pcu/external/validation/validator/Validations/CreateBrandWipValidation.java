package com.gdn.partners.pcu.external.validation.validator.Validations;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

import org.apache.commons.lang3.StringUtils;

import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.validation.validator.Annotations.CreateBrandWipValid;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;

public class CreateBrandWipValidation implements ConstraintValidator<CreateBrandWipValid, CreateBrandWipRequest> {

  private static final String VALID_CHARACTERS_REGEX = "[\\x20-\\x7E]+";

  @Override
  public void initialize(CreateBrandWipValid createBrandWipValid) {

  }

  @Override
  public boolean isValid(CreateBrandWipRequest createBrandWipRequest,
      ConstraintValidatorContext constraintValidatorContext) {
    if (!createBrandWipRequest.getBrandName().matches(VALID_CHARACTERS_REGEX)) {
      constraintValidatorContext.disableDefaultConstraintViolation();
      constraintValidatorContext
          .buildConstraintViolationWithTemplate(ErrorMessages.CREATE_BRAND_INVALID_CHARACTER_ERR_MESSAGE)
          .addConstraintViolation();
      return false;
    }
    return StringUtils.isNoneEmpty(createBrandWipRequest.getBrandDescription());
  }
}
