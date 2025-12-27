package com.gdn.x.product.domain.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Category implements Serializable {

  private static final long serialVersionUID = 8553760651590919217L;

  private String categoryCode;

  private String catgroupId;

  private int sequence;

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

  /**
   * @deprecated field sequence is unused
   */
  @Deprecated
  public int getSequence() {
    return this.sequence;
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

  /**
   * @deprecated field sequence is unused
   */
  @Deprecated
  public void setSequence(int sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String.format("Category [categoryCode=%s, catgroupId=%s, toString()=%s]",
        this.categoryCode, this.catgroupId, super.toString());
  }

}
