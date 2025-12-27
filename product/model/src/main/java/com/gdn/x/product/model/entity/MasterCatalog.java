package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class MasterCatalog implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.CATALOG_CODE)
  private String catalogCode;

  @Field(value = ProductFieldNames.CATEGORY)
  private Category category;

  public MasterCatalog() {

  }

  public MasterCatalog(String catalogCode, Category category) {
    super();
    this.catalogCode = catalogCode;
    this.category = category;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getCatalogCode() {
    return this.catalogCode;
  }

  public Category getCategory() {
    return this.category;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setCatalogCode(String catalogCode) {
    this.catalogCode = catalogCode;
  }

  public void setCategory(Category category) {
    this.category = category;
  }

  @Override
  public String toString() {
    return String.format("MasterCatalog [catalogCode=%s, category=%s, toString()=%s]",
        this.catalogCode, this.category, super.toString());
  }

}
