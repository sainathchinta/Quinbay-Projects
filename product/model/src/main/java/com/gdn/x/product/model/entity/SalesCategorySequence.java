package com.gdn.x.product.model.entity;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

public class SalesCategorySequence implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.CATEGORY_CODE)
  private String categoryCode;

  @Field(value = ProductFieldNames.SEQUENCE)
  private int sequence;

  public SalesCategorySequence() {}


  public SalesCategorySequence(String categoryCode, int sequence) {
    super();
    this.categoryCode = categoryCode;
    this.sequence = sequence;
  }


  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getCategoryCode() {
    return this.categoryCode;
  }

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

  public void setSequence(int sequence) {
    this.sequence = sequence;
  }

  @Override
  public String toString() {
    return String.format("SalesCategorySequence [categoryCode=%s, sequence=%s, toString()=%s]",
        this.categoryCode, this.sequence, super.toString());
  }
}
