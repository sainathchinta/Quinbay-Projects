package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterCatalog implements Serializable {

  private static final long serialVersionUID = -3831020770892883394L;

  private String catalogCode;

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
