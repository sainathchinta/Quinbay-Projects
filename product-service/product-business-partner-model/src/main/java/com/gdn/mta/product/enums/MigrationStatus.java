package com.gdn.mta.product.enums;

public enum MigrationStatus {
  NO_MIGRATION("NO_MIGRATION"),
  PENDING("PENDING"),
  IN_PROGRESS("IN_PROGRESS"),
  COMPLETED("COMPLETED"),
  UPDATE_FAILED("UPDATE_FAILED"),
  UPDATE_COMPLETED("UPDATED"),
  FAILED("FAILED");

  private final String migrationStatus;

  MigrationStatus(String migrationStatus) {
    this.migrationStatus = migrationStatus;
  }

  public String getMigrationStatus() {
    return migrationStatus;
  }
}
