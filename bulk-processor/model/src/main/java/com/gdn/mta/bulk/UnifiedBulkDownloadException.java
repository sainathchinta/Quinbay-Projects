package com.gdn.mta.bulk;

public class UnifiedBulkDownloadException extends Exception {

  private String errorCode;
  private String errorMessage;

  public UnifiedBulkDownloadException(String errorCode, String errorMessage) {
    this.errorCode = errorCode;
    this.errorMessage = errorMessage;
  }

  public String getErrorCode() {
    return errorCode;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

}
