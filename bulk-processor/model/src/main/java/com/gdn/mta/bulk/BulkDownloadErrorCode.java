package com.gdn.mta.bulk;

/**
 * Created by keshashah on 15/11/16.
 */
public enum BulkDownloadErrorCode {

  ENTITY_NOT_FOUND("Entity Does Not Found"),
  DATA_NOT_FOUND("Response is Empty"),
  ERROR_GENERATING_FILE("Error Generating file"),
  RECORD_NOT_FOUND("Record Doesn't Exist"),
  DOWNLOAD_IN_PROGRESS("Download is in progress");


  private String errorMessage;

  BulkDownloadErrorCode(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  public String getErrorMessage() {
    return errorMessage;
  }
}
