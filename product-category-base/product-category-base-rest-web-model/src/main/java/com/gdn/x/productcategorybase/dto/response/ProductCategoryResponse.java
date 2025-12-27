package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCategoryResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -4929347130497673350L;

  private CategoryResponse category;

  public ProductCategoryResponse() {}

  public ProductCategoryResponse(CategoryResponse category, String storeId) {
    this.category = category;
    this.setStoreId(storeId);
  }

  public CategoryResponse getCategory() {
    return this.category;
  }

  public void setCategory(CategoryResponse category) {
    this.category = category;
  }

  @Override
  public String toString() {
    return String.format("ProductCategory [category=%ss, toString()=%s]", this.category, super.toString());
  }
}
