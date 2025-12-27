package com.gdn.x.product.enums;

public enum CurationStatus {

  NEED_CURATION("Need Curation", 1),
  APPROVED("Approved", 2),
  REJECTED("Rejected", 3),
  NONE("None", 4);

  private String description;
  private int value;

  CurationStatus(String description, int value) {
    this.description = description;
    this.value = value;
  }

  public int getValue() {
    return value;
  }

  public String getDescription() {
    return description;
  }

  public static CurationStatus fromValue(int value) {
    for (CurationStatus status : CurationStatus.values()) {
      if (status.getValue() == value) {
        return status;
      }
    }
    return CurationStatus.NONE;
  }

  public static boolean validCurationStatus(String curationStatus) {
    return CurationStatus.APPROVED.name().equals(curationStatus) || CurationStatus.REJECTED.name()
        .equals(curationStatus);
  }
}
