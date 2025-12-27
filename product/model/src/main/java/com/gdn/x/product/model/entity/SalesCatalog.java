package com.gdn.x.product.model.entity;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class SalesCatalog implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.CATALOG_CODE)
  private String catalogCode;

  @Field(value = ProductFieldNames.LIST_OF_CATEGORIES)
  private List<Category> listOfCategories = new ArrayList<Category>();

  public SalesCatalog() {

  }

  public SalesCatalog(String catalogCode) {
    this.catalogCode = catalogCode;
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
