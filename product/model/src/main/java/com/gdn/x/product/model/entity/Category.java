package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class Category implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.CATEGORY_CODE)
  private String categoryCode;

  @Field(value = ProductFieldNames.CATGROUP_ID)
  private String catgroupId;

  public Category() {}

  public Category(String categoryCode, String catgroupId) {
    super();
    this.categoryCode = categoryCode;
    this.catgroupId = catgroupId;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
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
    return String.format("Category [categoryCode=%s, catgroupId=%s, toString()=%s]",
        this.categoryCode, this.catgroupId, super.toString());
  }
}
