package com.gdn.mta.product.valueobject;

/**
 * Created by sarang on 06/04/17.
 */
public class SortOrder {

  private String orderBy;
  private String sortBy;

  public SortOrder() {
  }

  public SortOrder(String orderBy, String sortBy) {
    this.orderBy = orderBy;
    this.sortBy = sortBy;
  }

  public String getOrderBy() {
    return orderBy;
  }

  public void setOrderBy(String orderBy) {
    this.orderBy = orderBy;
  }

  public String getSortBy() {
    return sortBy;
  }

  public void setSortBy(String sortBy) {
    this.sortBy = sortBy;
  }

  @Override public String toString() {
    return "SortOrder{" + "orderBy='" + orderBy + '\'' + ", sortBy='" + sortBy + '\'' + '}';
  }

}