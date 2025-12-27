package com.gdn.mta.bulk;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
public class ValidationException extends RuntimeException {

  private String errorCode;
  private String errorMessage;

  public ValidationException(String errorCode, String errorMessage) {
    this.errorCode = errorCode;
    this.errorMessage = errorMessage;
  }
}

