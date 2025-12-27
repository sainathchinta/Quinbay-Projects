package com.gdn.x.product.enums;

public enum DistributionStatus {

  NON_DISTRIBUTION("NON_DISTRIBUTION", 0),
  PURE_DISTRIBUTION("PURE_DISTRIBUTION", 1),
  DISTRIBUTION("DISTRIBUTION", 2);
  private String description;
  private int code;

   DistributionStatus(String description, int code) {
    this.description = description;
    this.code = code;
  }

  public int getCode() {
    return code;
  }

  public String getDescription() {
    return description;
  }

  public static DistributionStatus getByCode(int code) {
    for (DistributionStatus status : DistributionStatus.values()) {
      if (status.getCode() == code) {
        return status;
      }
    }
    return DistributionStatus.NON_DISTRIBUTION;
  }
}
