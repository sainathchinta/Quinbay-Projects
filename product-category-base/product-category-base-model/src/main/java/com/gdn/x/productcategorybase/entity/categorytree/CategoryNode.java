package com.gdn.x.productcategorybase.entity.categorytree;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class CategoryNode implements Serializable {

  private static final long serialVersionUID = -5791040556665973649L;
  private String categoryCode;
  private String categoryName;
  private String parentCategoryCode;
  private boolean active = false;
  private Long childCount;

  public CategoryNode() {}

  public CategoryNode(String categoryCode, String categoryName, String parentCategoryCode, boolean active,
      Long childCount) {
    super();
    this.categoryCode = categoryCode;
    this.categoryName = categoryName;
    this.parentCategoryCode = parentCategoryCode;
    this.active = active;
    this.childCount = childCount;
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

  @Override
  public String toString() {
    return String.format(
        "CategoryNode [categoryCode=%s, categoryName=%s, parentCategoryCode=%s, active=%s, childCount=%s]",
        categoryCode, categoryName, parentCategoryCode, active, childCount);
  }

}
