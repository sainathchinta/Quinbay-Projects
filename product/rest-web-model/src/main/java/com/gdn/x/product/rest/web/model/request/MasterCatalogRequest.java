package com.gdn.x.product.rest.web.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterCatalogRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private String catalogCode;
  private CategoryDTO category;

  public MasterCatalogRequest() {

  }

  public MasterCatalogRequest(String catalogCode, CategoryDTO category) {
    this.catalogCode = catalogCode;
    this.category = category;
  }

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }

  public String getCatalogCode() {
    return catalogCode;
  }

  public CategoryDTO getCategory() {
    return category;
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
    return String.format("MasterCatalogRequest [catalogCode=%s, category=%s, toString()=%s]",
        catalogCode, category, super.toString());
  }

}
