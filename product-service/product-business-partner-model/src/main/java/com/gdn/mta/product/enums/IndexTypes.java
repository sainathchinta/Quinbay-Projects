package com.gdn.mta.product.enums;

public enum IndexTypes {

  FULL_REINDEX("fullReindex"),
  DELTA_REINDEX("deltaReindex");

  private final String indexType;

  IndexTypes(String indexType) {
    this.indexType = indexType;
  }

  public String getIndexType() {
    return indexType;
  }
}
