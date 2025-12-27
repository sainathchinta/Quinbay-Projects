package com.gdn.x.product.exception;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;

public class PristineDataItemNotFoundException extends ApplicationRuntimeException {

  public PristineDataItemNotFoundException(String message) {
    super(ErrorCategory.DATA_NOT_FOUND, message);
  }

}

