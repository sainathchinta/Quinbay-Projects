package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.gdn.common.base.GdnObjects;

/**
 * Created by govind on 05/02/2019 AD.
 */
public class ItemCategoryVO implements Serializable{

  private static final long serialVersionUID = -5900965837774515905L;
  private int level;
  private String categoryId;
  private String category;
  private String productCategoryCode;

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

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("level", level).append("categoryId", categoryId)
        .append("category", category).append("productCategoryCode", productCategoryCode).toString();
  }
}
