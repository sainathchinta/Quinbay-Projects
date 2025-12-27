package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCategoryRequest extends BaseDTORequest {

  private static final long serialVersionUID = -4929347130497673350L;

  private CategoryRequest category;

  public ProductCategoryRequest() {}

  public ProductCategoryRequest(CategoryRequest category, String storeId) {
    this.category = category;
    this.setStoreId(storeId);
  }

  public CategoryRequest getCategory() {
    return this.category;
  }

  public void setCategory(CategoryRequest category) {
    this.category = category;
  }

  @Override
  public String toString() {
    return String.format("ProductCategory [category=%ss, toString()=%s]", this.category, super.toString());
  }
}
