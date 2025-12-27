package com.gdn.mta.product.service.exception;

import com.gdn.mta.product.enums.ApiErrorCode;
import lombok.Data;

@Data
public class ApiInvalidImageQcConfigException extends ApiGenericException {
  private static final long serialVersionUID = 8123498152659801611L;

  public ApiInvalidImageQcConfigException(String errorMessage, ApiErrorCode errorCode) {
    super(ApiDataNotFoundException.class.getSimpleName(), errorMessage, errorCode);
  }
}
