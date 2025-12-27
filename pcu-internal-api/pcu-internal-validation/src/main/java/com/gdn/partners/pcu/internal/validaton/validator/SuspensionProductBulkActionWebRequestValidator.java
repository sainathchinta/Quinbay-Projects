package com.gdn.partners.pcu.internal.validaton.validator;


import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.validaton.annotation.SuspensionProductBulkActionsWebRequestValid;
import com.gdn.partners.pcu.internal.web.model.request.SuspensionProductBulkActionsWebRequest;

public class SuspensionProductBulkActionWebRequestValidator implements
    ConstraintValidator<SuspensionProductBulkActionsWebRequestValid, SuspensionProductBulkActionsWebRequest> {

  private static final String SUSPEND_ACTION = "SUSPEND";
  private static final String REACTIVATE_ACTION = "REACTIVATE";

  private boolean validateUpdate(SuspensionProductBulkActionsWebRequest request, ConstraintValidatorContext context) {
    if (StringUtils.isEmpty(request.getNotes())) {
      context.buildConstraintViolationWithTemplate(ErrorMessages.NOTES_TYPE_EMPTY).addConstraintViolation();
      return false;
    }

    if (StringUtils.isEmpty(request.getReason())) {
      context.buildConstraintViolationWithTemplate(ErrorMessages.REASON_TYPE_EMPTY).addConstraintViolation();
      return false;
    }

    if (!(SUSPEND_ACTION.equals(request.getAction()) || REACTIVATE_ACTION.equals(request.getAction()))) {
      context.buildConstraintViolationWithTemplate(ErrorMessages.ACTION_TYPE_INVALID).addConstraintViolation();
      return false;
    }

    if (CollectionUtils.isEmpty(request.getProducts())) {
      context.buildConstraintViolationWithTemplate(ErrorMessages.EMPTY_PRODUCT_LIST).addConstraintViolation();
      return false;
    }
    return true;
  }

  @Override
  public void initialize(SuspensionProductBulkActionsWebRequestValid constraintAnnotation) {

  }

  @Override
  public boolean isValid(SuspensionProductBulkActionsWebRequest value, ConstraintValidatorContext context) {
    return validateUpdate(value, context);
  }
}
