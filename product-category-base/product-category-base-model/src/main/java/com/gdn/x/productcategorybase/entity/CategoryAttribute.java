package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

@Entity
@Table(name = CategoryAttribute.TABLE_NAME)
public class CategoryAttribute extends GdnBaseEntity {

  private static final long serialVersionUID = -3416137271364410851L;
  public static final String TABLE_NAME = "PCC_CATEGORY_ATTRIBUTE";
  public static final String COLUMN_CATEGORY_ID = "CATEGORY_ID";
  public static final String COLUMN_ATTRIBUTE_ID = "ATTRIBUTE_ID";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";
  public static final String COLUMN_IS_MAIN_DEFINING_ATTRIBUTE = "IS_MAIN_DEFINING_ATTRIBUTE";
  public static final String COLUMN_IS_USP = "IS_USP";

  @ManyToOne
  @JoinColumn(name = CategoryAttribute.COLUMN_CATEGORY_ID)
  private Category category;

  @ManyToOne
  @JoinColumn(name = CategoryAttribute.COLUMN_ATTRIBUTE_ID)
  private Attribute attribute;

  @Column(name = CategoryAttribute.COLUMN_SEQUENCE)
  private Integer sequence;

  @Column(name = CategoryAttribute.COLUMN_IS_MAIN_DEFINING_ATTRIBUTE)
  private boolean isMainDefiningAttribute = false;

  @Column(name = CategoryAttribute.COLUMN_IS_USP)
  private boolean isUSP = false;

  public CategoryAttribute() {
    // nothing to do here
  }

  public CategoryAttribute(Category category, Attribute attribute, Integer sequence, boolean isMainDefiningAttribute,
      boolean isUSP, String storeId) {
    this.category = category;
    this.attribute = attribute;
    this.sequence = sequence;
    this.isMainDefiningAttribute = isMainDefiningAttribute;
    this.isUSP = isUSP;
    this.setStoreId(storeId);
  }

  public Attribute getAttribute() {
    return this.attribute;
  }

  public Category getCategory() {
    return this.category;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public boolean isMainDefiningAttribute() {
    return this.isMainDefiningAttribute;
  }

  public boolean isUSP() {
    return this.isUSP;
  }

  public void setAttribute(Attribute attribute) {
    this.attribute = attribute;
  }

  public void setCategory(Category category) {
    this.category = category;
  }

  public void setMainDefiningAttribute(boolean isMainDefiningAttribute) {
    this.isMainDefiningAttribute = isMainDefiningAttribute;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setUSP(boolean isUSP) {
    this.isUSP = isUSP;
  }

  @Override
  public String toString() {
    return String.format("CategoryAttribute [sequence=%s, isMainDefiningAttribute=%s, isUSP=%s, toString()=%s]",
        this.sequence, this.isMainDefiningAttribute, this.isUSP, super.toString());
  }

}
