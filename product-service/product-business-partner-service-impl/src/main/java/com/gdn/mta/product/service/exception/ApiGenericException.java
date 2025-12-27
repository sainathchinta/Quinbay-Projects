package com.gdn.mta.product.service.exception;

import com.gdn.mta.product.enums.ApiErrorCode;

import lombok.Data;

/**
 * This exception is used as generic exception of X-Bulk. It will replace Generic Exception usage and
 * deprecated further
 *
 * @author shivam.gupta
 */
@Data
public class ApiGenericException extends RuntimeException {

  private static final long serialVersionUID = -6912505547124355582L;

  private String exceptionType;
  private String errorMsg;
  private ApiErrorCode errorCode;

  public ApiGenericException(String exceptionType, String errorMsg, ApiErrorCode errorCode) {
    super(errorMsg);
    this.exceptionType = exceptionType;
    this.errorMsg = errorMsg;
    this.errorCode = errorCode;
  }
}