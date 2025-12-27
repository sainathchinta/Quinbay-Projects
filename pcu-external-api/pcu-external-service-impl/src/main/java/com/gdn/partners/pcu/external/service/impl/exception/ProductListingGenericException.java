package com.gdn.partners.pcu.external.service.impl.exception;

public class ProductListingGenericException extends RuntimeException{
  private static final long serialVersionUID = 5667195586356788303L;
  public String getErrorCode() {
    return errorCode;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  private String errorCode;
  private String errorMessage;

  public ProductListingGenericException(String errorCode, String errorMessage) {
    this.errorCode = errorCode;
    this.errorMessage = errorMessage;
  }
}
