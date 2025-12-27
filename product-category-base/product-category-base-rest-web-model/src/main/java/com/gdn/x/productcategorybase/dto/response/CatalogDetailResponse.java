package com.gdn.x.productcategorybase.dto.response;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CatalogDetailResponse extends CatalogResponse {

  private static final long serialVersionUID = -3968356407851803325L;

  private List<CategoryResponse> categories;

  public CatalogDetailResponse() {
    // nothing to do here
  }

  public CatalogDetailResponse(List<CategoryResponse> categories) {
    super();
    this.categories = categories;
  }

  public List<CategoryResponse> getCategories() {
    return this.categories;
  }

  public void setCategories(List<CategoryResponse> categories) {
    this.categories = categories;
  }

  @Override
  public String toString() {
    return String.format("CatalogDetailResponse [categories=%s, toString()=%s]", this.categories, super.toString());
  }

}
