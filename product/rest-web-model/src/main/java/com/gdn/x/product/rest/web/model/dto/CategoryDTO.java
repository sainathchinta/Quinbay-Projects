package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryDTO implements Serializable {
  private static final long serialVersionUID = 1L;

  private String categoryCode;
  private String catgroupId;

  public CategoryDTO() {

  }

  public CategoryDTO(String categoryCode, String catgroupId) {
    this.categoryCode = categoryCode;
    this.catgroupId = catgroupId;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public String getCategoryCode() {
    return this.categoryCode;
  }

  public String getCatgroupId() {
    return this.catgroupId;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setCatgroupId(String catgroupId) {
    this.catgroupId = catgroupId;
  }

  @Override
  public String toString() {
    return String.format("CategoryDTO [categoryCode=%s, catgroupId=%s, toString()=%s]",
        this.categoryCode, this.catgroupId, super.toString());
  }
}
