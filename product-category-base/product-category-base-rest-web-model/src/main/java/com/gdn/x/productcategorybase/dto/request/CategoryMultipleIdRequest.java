package com.gdn.x.productcategorybase.dto.request;

import java.util.List;

public class CategoryMultipleIdRequest {
  private List<String> categoryCode;

  public CategoryMultipleIdRequest() {

  }

  public List<String> getCategoryCode() {
    return this.categoryCode;
  }

  public void setCategoryCode(List<String> categoryCode) {
    this.categoryCode = categoryCode;
  }

  @Override
  public String toString() {
    return "CategoryMultipleIdRequest [categoryCode=" + this.categoryCode + "]";
  }
}
