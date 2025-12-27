package com.gdn.x.mta.distributiontask.model.enums;

public enum DistributionStatus {

  NON_DISTRIBUTION("NON_DISTRIBUTION", 0),
  PURE_DISTRIBUTION("PURE_DISTRIBUTION", 1),
  DISTRIBUTION("DISTRIBUTION", 2);
  private final String description;
  private final int code;

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
}
