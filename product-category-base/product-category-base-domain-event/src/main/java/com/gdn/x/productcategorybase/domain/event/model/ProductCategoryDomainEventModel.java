package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCategoryDomainEventModel {

  CategoryDomainEventModel category;

  public ProductCategoryDomainEventModel() {
    // do nothing
  }

  public ProductCategoryDomainEventModel(CategoryDomainEventModel category) {
    super();
    this.category = category;
  }

  public CategoryDomainEventModel getCategory() {
    return category;
  }

  public void setCategory(CategoryDomainEventModel category) {
    this.category = category;
  }

  @Override
  public String toString() {
    return String.format("ProductCategoryDomainEventModel [category=%s, getCategory()=%s]", category, getCategory());
  }

}
