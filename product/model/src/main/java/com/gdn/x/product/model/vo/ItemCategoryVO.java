package com.gdn.x.product.model.vo;

import java.io.Serializable;

import com.gdn.common.base.GdnObjects;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ItemCategoryVO implements Serializable {

  private static final long serialVersionUID = 1L;
  private int level;
  private String categoryId;
  private String category;
  private String productCategoryCode;
  private String documentType;
  private boolean activated;
  private boolean categoryActive;
  private boolean display;

  public ItemCategoryVO() {}

  public ItemCategoryVO(int level, String categoryId, String category, String productCategoryCode) {
    this.level = level;
    this.categoryId = categoryId;
    this.category = category;
    this.productCategoryCode = productCategoryCode;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }


  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }


  @Override
  public String toString() {
    return "ItemCategoryVO [level=" + this.level + ", categoryId=" + this.categoryId + ", category="
        + this.category + ", productCategoryCode=" + this.productCategoryCode + ", active="
        + this.activated + ", categoryActive=" + this.categoryActive + ", display=" + this.display
        + ", toString()=" + super.toString() + "]";
  }

}
