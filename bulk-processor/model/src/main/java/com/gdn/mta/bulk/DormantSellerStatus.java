package com.gdn.mta.bulk;

public enum DormantSellerStatus {
  PENDING("PENDING"),
  IN_PROGRESS("IN_PROGRESS"),
  COMPLETED("COMPLETED"),
  PROCESSING("PROCESSING"),
  PARTIAL_COMPLETED("PARTIAL_COMPLETED"),
  FAILED("FAILED"),
  SKIPPED("SKIPPED"),
  FETCHED("FETCHED");

  private final String status;

  DormantSellerStatus(String status) {
    this.status = status;
  }
}
