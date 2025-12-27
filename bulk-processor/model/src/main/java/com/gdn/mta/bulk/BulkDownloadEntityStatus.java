package com.gdn.mta.bulk;

/**
 * Created by virajjasani on 08/09/16.
 */
public enum BulkDownloadEntityStatus {

  STATUS_PENDING("PENDING"),
  STATUS_IN_PROGRESS("IN_PROGRESS"),
  STATUS_SUCCESS("SUCCESS"),
  STATUS_FAILED("FAILED");

  private String statusValue;

  BulkDownloadEntityStatus(String statusValue) {
    this.statusValue = statusValue;
  }

  public String getStatusValue() {
    return statusValue;
  }
}
