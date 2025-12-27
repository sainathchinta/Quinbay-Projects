package com.gdn.mta.bulk;

/**
 * Created by keshashah on 15/11/16.
 */
public class BulkDownloadException extends Exception {
  private static final long serialVersionUID = -10077605855139456L;
  private String errorCode;
  private String errorMessage;

  public BulkDownloadException(String errorCode, String errorMessage) {
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
