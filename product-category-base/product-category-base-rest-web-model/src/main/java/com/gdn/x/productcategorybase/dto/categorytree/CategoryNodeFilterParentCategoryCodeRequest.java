package com.gdn.x.productcategorybase.dto.categorytree;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryNodeFilterParentCategoryCodeRequest implements Serializable {

  private static final long serialVersionUID = -8590685620597591720L;
  private String catalogCode;
  private String parentCategoryCode;
  private boolean active = false;

  public CategoryNodeFilterParentCategoryCodeRequest() {}

  public CategoryNodeFilterParentCategoryCodeRequest(String catalogCode, String parentCategoryCode, boolean active) {
    super();
    this.catalogCode = catalogCode;
    this.parentCategoryCode = parentCategoryCode;
    this.active = active;
  }

  public String getCatalogCode() {
    return catalogCode;
  }

  public void setCatalogCode(String catalogCode) {
    this.catalogCode = catalogCode;
  }

  public String getParentCategoryCode() {
    return parentCategoryCode;
  }

  public void setParentCategoryCode(String parentCategoryCode) {
    this.parentCategoryCode = parentCategoryCode;
  }

  public boolean isActive() {
    return active;
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  @Override
  public String toString() {
    return "CategoryNodeFilterParentCategoryCodeRequest [catalogCode=" + catalogCode + ", parentCategoryCode="
        + parentCategoryCode + ", active=" + active + "]";
  }

}
