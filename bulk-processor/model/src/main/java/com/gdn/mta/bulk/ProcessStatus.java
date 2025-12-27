package com.gdn.mta.bulk;

public enum ProcessStatus {
  PENDING("PENDING"),
  IN_PROGRESS("IN_PROGRESS"),
  PUBLISHED("PUBLISHED"),
  COMPLETED("COMPLETED"),
  PARTIAL_COMPLETED("PARTIAL_COMPLETED"),
  FAILED("FAILED"),
  FINISHED("FINISHED"),
  CANCELLED("CANCELLED"),
  PROCESSING("PROCESSING"),
  PICKED("PICKED");

  private final String status;

  ProcessStatus(String status) {
    this.status = status;
  }
}
