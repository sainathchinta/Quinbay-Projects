package com.gdn.partners.pcu.internal.web.model.enums;

public enum SellerStatus {

  REJECT("REJECTED"), TERMINATE("TERMINATED"), SUSPENDED("SUSPENDED"), RESIGN("RESIGNED"), INACTIVE(
    "INACTIVE");

  private final String status;

  private SellerStatus(String status) {
    this.status = status;
  }

  public String getStatus() {
    return status;
  }

  public static String populateSellerStatus(String status) {
    for (SellerStatus sellerStatus : SellerStatus.values()) {
      if (sellerStatus.name().equals(status)) {
        return sellerStatus.getStatus();
      }
    }
    return null;
  }
}
