package com.gdn.mta.product.service.exception;

import com.gdn.mta.product.enums.ApiErrorCode;

/**
 * This exception is used to handle data not found issue
 * @author shivam.gupta
 *
 */
public class ApiDataNotFoundException extends ApiGenericException {

  public ApiDataNotFoundException(String errorMessage, ApiErrorCode errorCode) {
    super(ApiDataNotFoundException.class.getSimpleName(), errorMessage, errorCode);
  }
}