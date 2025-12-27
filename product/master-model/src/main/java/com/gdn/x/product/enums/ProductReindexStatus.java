package com.gdn.x.product.enums;

public enum ProductReindexStatus {

  REINDEX_PENDING("Reindex Pending"),

  REINDEX_STARTED("Reindex started"),

  REINDEX_PENDING_FLAGS("Reindex Pending Flags"),

  REINDEX_FLAGS_FAILED("Reindex Flags Failed"),

  REINDEX_SUCCESS("Reindex Success"),

  REINDEX_FAILED("Reindex failed"),

  FULL_REINDEX_PENDING_PRODUCTS("Reindex Pending Products"),

  FULL_REINDEX_PENDING_PRODUCTS_FAILED("Reindex Pending Failed"),

  REINDEX_PENDING_L3("Reindex Pending L3 Products"),

  REINDEX_PENDING_L3_SUCCESS("Reindex Success For Pending L3 Products"),

  REINDEX_PENDING_L3_FAILED("Reindex Failed For Pending L3 Products"),

  REINDEX_PENDING_L3_SOLR_DB("Reindex Pending L3 Products in database and solr"),

  REINDEX_PENDING_L3_SOLR_DB_SUCCESS("Reindex Success For Pending L3 Products"),

  REINDEX_PENDING_L3_SOLR_DB_FAILED("Reindex Failed For Pending L3 Products");

  private String description;

  private ProductReindexStatus(String description) {
    this.description = description;
  }

  public String getDescription() {
    return description;
  }
}
