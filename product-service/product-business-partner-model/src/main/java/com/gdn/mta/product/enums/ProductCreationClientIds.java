package com.gdn.mta.product.enums;

public enum ProductCreationClientIds {
  MTA_APP("MTAApp"), MTA_API("mta-api"), PCU_EXTERNAL_APP("pcu-external-app"), X_BULK("x-bulk");

  private final String ProductCreationClientId;

  ProductCreationClientIds(String ProductCreationClientId) {
    this.ProductCreationClientId = ProductCreationClientId;
  }

  public String getProductCreationClientId() {
    return ProductCreationClientId;
  }
}
