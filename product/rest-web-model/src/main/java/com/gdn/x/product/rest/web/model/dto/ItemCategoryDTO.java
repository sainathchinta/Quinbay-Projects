package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemCategoryDTO implements Serializable {

  private static final long serialVersionUID = 5938298479208445152L;

  private int level;
  private String categoryId;
  private String category;
  private String productCategoryCode;
  private boolean categoryActive;
  private boolean display;

  public ItemCategoryDTO() {}

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  public String getCategory() {
    return this.category;
  }

  public String getCategoryId() {
    return this.categoryId;
  }

  public int getLevel() {
    return this.level;
  }

  public String getProductCategoryCode() {
    return this.productCategoryCode;
  }

  public boolean getCategoryActive() {
    return this.categoryActive;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setCategory(String category) {
    this.category = category;
  }

  public void setCategoryId(String categoryId) {
    this.categoryId = categoryId;
  }

  public void setLevel(int level) {
    this.level = level;
  }

  public void setProductCategoryCode(String productCategoryCode) {
    this.productCategoryCode = productCategoryCode;
  }

  public void setCategoryActive(boolean categoryActive) {
    this.categoryActive = categoryActive;
  }

  public boolean isDisplay() {
    return display;
  }

  public void setDisplay(boolean display) {
    this.display = display;
  }

  @Override
  public String toString() {
    return "ItemCategoryDTO [level=" + this.level + ", categoryId=" + this.categoryId + ", category=" + this.category
        + ", productCategoryCode=" + this.productCategoryCode + ", categoryActive=" + this.categoryActive + ", display="
        + this.display + ", toString()=" + super.toString() + "]";
  }
}
