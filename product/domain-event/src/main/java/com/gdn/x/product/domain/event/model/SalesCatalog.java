package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SalesCatalog implements Serializable {

  private static final long serialVersionUID = 2426814145628019557L;

  private String catalogCode;

  private List<Category> listOfCategories = new ArrayList<Category>();

  public SalesCatalog() {

  }

  public SalesCatalog(String catalogCode, List<Category> listOfCategories) {
    super();
    this.catalogCode = catalogCode;
    this.listOfCategories = listOfCategories;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getCatalogCode() {
    return this.catalogCode;
  }

  public List<Category> getListOfCategories() {
    return this.listOfCategories;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setCatalogCode(String catalogCode) {
    this.catalogCode = catalogCode;
  }

  public void setListOfCategories(List<Category> listOfCategories) {
    this.listOfCategories = listOfCategories;
  }

  @Override
  public String toString() {
    return String.format("SalesCatalog [catalogCode=%s, listOfCategories=%s, toString()=%s]",
        this.catalogCode, this.listOfCategories, super.toString());
  }

}
