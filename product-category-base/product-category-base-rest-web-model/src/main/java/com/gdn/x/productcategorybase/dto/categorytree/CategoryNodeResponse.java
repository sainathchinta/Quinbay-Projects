package com.gdn.x.productcategorybase.dto.categorytree;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryNodeResponse extends BaseResponse {

  private static final long serialVersionUID = 17747984906113132L;
  private String categoryCode;
  private String categoryName;
  private String parentCategoryCode;
  private boolean active = false;
  private Long childCount;
  private List<CategoryNodeResponse> children = new ArrayList<CategoryNodeResponse>();

  public CategoryNodeResponse() {}

  public CategoryNodeResponse(String categoryCode, String categoryName, String parentCategoryCode, boolean active,
      Long childCount) {
    super();
    this.categoryCode = categoryCode;
    this.categoryName = categoryName;
    this.parentCategoryCode = parentCategoryCode;
    this.active = active;
    this.childCount = childCount;
  }

  public CategoryNodeResponse(String categoryCode, String categoryName, String parentCategoryCode, boolean active,
      Long childCount, List<CategoryNodeResponse> children) {
    this(categoryCode, categoryName, parentCategoryCode, active, childCount);
    this.children = children;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
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

  public Long getChildCount() {
    return childCount;
  }

  public void setChildCount(Long childCount) {
    this.childCount = childCount;
  }

  public List<CategoryNodeResponse> getChildren() {
    return children;
  }

  public void setChildren(List<CategoryNodeResponse> children) {
    this.children = children;
  }

  @Override
  public String toString() {
    return String
        .format(
            "CategoryNodeResponse [categoryCode=%s, categoryName=%s, parentCategoryCode=%s, active=%s, childCount=%s, children=%s]",
            categoryCode, categoryName, parentCategoryCode, active, childCount, children);
  }

}
