package com.gdn.partners.pcu.master.service.impl.exception;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ValidationException extends RuntimeException {
  String errorCode;

  public ValidationException(String message) {
    super(message);
  }

  public ValidationException(String errorCode, String errorMessage) {
    super(errorMessage);
    this.errorCode = errorCode;
  }
}
