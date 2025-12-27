package com.gdn.mta.product.service.exception;

import com.gdn.mta.product.enums.ApiErrorCode;
import lombok.Data;

@Data
public class ApiIncorrectInputDataException extends ApiGenericException {

  private static final long serialVersionUID = 8123498152659801601L;

  public ApiIncorrectInputDataException(String errorMessage, ApiErrorCode errorCode) {
    super(ApiIncorrectInputDataException.class.getSimpleName(), errorMessage, errorCode);
  }
}
