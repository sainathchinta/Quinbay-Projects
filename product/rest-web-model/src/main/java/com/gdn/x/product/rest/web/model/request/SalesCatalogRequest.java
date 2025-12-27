package com.gdn.x.product.rest.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SalesCatalogRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private String catalogCode;
  private List<CategoryDTO> listOfCategories;

  public SalesCatalogRequest() {

  }

  public SalesCatalogRequest(String catalogCode, List<CategoryDTO> listOfCategories) {
    this.catalogCode = catalogCode;
    this.listOfCategories = listOfCategories;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getCatalogCode() {
    return catalogCode;
  }

  public List<CategoryDTO> getListOfCategories() {
    return listOfCategories;
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
    return String.format(
        "SalesCatalogRequest [catalogCode=%s, listOfCategories=%s, toString()=%s]", catalogCode,
        listOfCategories, super.toString());
  }
}
