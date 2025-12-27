package com.gdn.mta.bulk.exception;

import com.gdn.mta.bulk.ApiErrorCode;

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
