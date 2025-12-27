package com.gdn.partners.product.analytics.service.impl.helper;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.google.common.base.Preconditions;

public class RequestHelper {

  public static void checkArgument(boolean expression, String errorMessage) {
    try {
      Preconditions.checkArgument(expression, errorMessage);
    } catch (IllegalArgumentException e) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, errorMessage, e);
    }
  }
}
