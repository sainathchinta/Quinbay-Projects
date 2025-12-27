package com.gdn.mta.product.enums;

public enum ProductCreationType {
  CATEGORY_BULK_UPLOAD("CATEGORY_BULK_UPLOAD"), UNIFIED_BULK_UPLOAD("UNIFIED_BULK_UPLOAD"), FLOW1_WEB("FLOW1_WEB"),
  FLOW1_APP("FLOW1_APP"), FLOW1_API("FLOW1_API"), FLOW2_WEB("FLOW2_WEB"), FLOW2_APP("FLOW2_APP"), FLOW2_API("FLOW2_API"),
  FLOW3_WEB("FLOW3_WEB"), FLOW1("FLOW1"), FLOW2("FLOW2"), FLOW3("FLOW3"), FLOW2_FBB("FLOW2_FBB"), MIGRATION("MIGRATION"), AUTO_UPLOAD(
      "AUTO_UPLOAD"), STORE_COPY_FLOW_2("STORE_COPY_FLOW_2"), EXTERNAL_BULK_UPLOAD(
      "EXTERNAL_BULK_UPLOAD"), CONVERTED_BULK_UPLOAD("CONVERTED_BULK_UPLOAD");

  private final String productCreationType;

  ProductCreationType(String productCreationType) {
    this.productCreationType = productCreationType;
  }

  public String getProductCreationType() {
    return productCreationType;
  }
}
