package com.gdn.partners.pbp.model;

import com.gdn.common.base.GdnObjects;

public class SortOrder {

  private String sortBy;
  private String sortType;

  public SortOrder() {}

  public SortOrder(String sortBy, String sortType) {
    super();
    this.sortBy = sortBy;
    this.sortType = sortType;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getSortBy() {
    return this.sortBy;
  }

  public String getSortType() {
    return this.sortType;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setSortBy(String sortBy) {
    this.sortBy = sortBy;
  }

  public void setSortType(String sortType) {
    this.sortType = sortType;
  }

  @Override
  public String toString() {
    return String.format("SortOrder [sortBy=%s, sortType=%s, toString()=%s]", this.sortBy,
        this.sortType, super.toString());
  }
}
