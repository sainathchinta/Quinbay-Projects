package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SalesCatalogDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private String catalogCode;
  private List<CategoryDTO> listOfCategories;

  public SalesCatalogDTO() {

  }

  public SalesCatalogDTO(String catalogCode, List<CategoryDTO> listOfCategories) {
    this.catalogCode = catalogCode;
    this.listOfCategories = listOfCategories;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getCatalogCode() {
    return this.catalogCode;
  }

  public List<CategoryDTO> getListOfCategories() {
    return this.listOfCategories;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setCatalogCode(String catalogCode) {
    this.catalogCode = catalogCode;
  }

  public void setListOfCategories(List<CategoryDTO> listOfCategories) {
    this.listOfCategories = listOfCategories;
  }

  @Override
  public String toString() {
    return String.format("SalesCatalogDTO [catalogCode=%s, listOfCategories=%s, toString()=%s]",
        this.catalogCode, this.listOfCategories, super.toString());
  }
}
