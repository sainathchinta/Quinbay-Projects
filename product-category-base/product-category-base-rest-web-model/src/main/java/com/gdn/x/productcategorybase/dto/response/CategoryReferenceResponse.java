package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryReferenceResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 1065910821027776103L;

  private CategoryResponse masterCategoryReference;

  private CategoryResponse salesCategoryReference;

  private CategoryResponse b2bSalesCategoryReference;

  private CategoryResponse halalSalesCategoryReference;

  public CategoryReferenceResponse() {}

  public CategoryReferenceResponse(CategoryResponse masterCategoryReference, CategoryResponse salesCategoryReference) {
    this.masterCategoryReference = masterCategoryReference;
    this.salesCategoryReference = salesCategoryReference;
  }

  public CategoryResponse getMasterCategoryReference() {
    return this.masterCategoryReference;
  }

  public CategoryResponse getSalesCategoryReference() {
    return this.salesCategoryReference;
  }

  public void setMasterCategoryReference(CategoryResponse masterCategoryReference) {
    this.masterCategoryReference = masterCategoryReference;
  }

  public void setSalesCategoryReference(CategoryResponse salesCategoryReference) {
    this.salesCategoryReference = salesCategoryReference;
  }

  public CategoryResponse getB2bSalesCategoryReference() {
    return b2bSalesCategoryReference;
  }

  public void setB2bSalesCategoryReference(CategoryResponse b2bSalesCategoryReference) {
    this.b2bSalesCategoryReference = b2bSalesCategoryReference;
  }

  public CategoryResponse getHalalSalesCategoryReference() {
    return halalSalesCategoryReference;
  }

  public void setHalalSalesCategoryReference(CategoryResponse halalSalesCategoryReference) {
    this.halalSalesCategoryReference = halalSalesCategoryReference;
  }

  @Override
  public String toString() {
    return "CategoryReferenceResponse{" + "masterCategoryReference=" + this.masterCategoryReference
        + ", salesCategoryReference=" + this.salesCategoryReference + ", b2bSalesCategoryReference="
        + this.b2bSalesCategoryReference + ", halalCategoryReference=" + this.halalSalesCategoryReference + '}';
  }
}
