package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterCatalogDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private String catalogCode;
  private CategoryDTO category;

  public MasterCatalogDTO() {

  }

  public MasterCatalogDTO(String catalogCode, CategoryDTO category) {
    this.catalogCode = catalogCode;
    this.category = category;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getCatalogCode() {
    return this.catalogCode;
  }

  public CategoryDTO getCategory() {
    return this.category;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setCatalogCode(String catalogCode) {
    this.catalogCode = catalogCode;
  }

  public void setCategory(CategoryDTO category) {
    this.category = category;
  }

  @Override
  public String toString() {
    return String.format("MasterCatalogDTO [catalogCode=%s, category=%s, toString()=%s]",
        this.catalogCode, this.category, super.toString());
  }
}
