package com.gdn.x.product.enums;

public enum ReindexType {

  ITEM_REINDEX("ITEM_REINDEX"),
  OFFLINE_ITEM_REINDEX("OFFLINE_ITEM_REINDEX"),
  DELETE_FROM_SOLR("DELETE_FROM_SOLR");

  private String description;

  ReindexType(String description) {
    this.description = description;
  }

  public String getDescription() {
    return this.description;
  }
}
