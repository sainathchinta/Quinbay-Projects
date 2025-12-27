package com.gdn.mta.product.enums;

public enum ImageQcStatus {
  PENDING("PENDING"),
  IN_PROGRESS("IN_PROGRESS"),
  COMPLETED("COMPLETED"),
  FAILED("FAILED");

  private final String imageQcStatus;

  ImageQcStatus(String imageQcStatus) {
    this.imageQcStatus = imageQcStatus;
  }

  public String getImageQcStatus() {
    return imageQcStatus;
  }
}