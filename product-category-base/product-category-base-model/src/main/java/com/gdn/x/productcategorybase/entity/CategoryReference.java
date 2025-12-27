package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;


@Entity
@Table(name = CategoryReference.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    CategoryReference.COLUMN_MASTER_CATEGORY_REFERENCE_ID, CategoryReference.COLUMN_SALES_CATEGORY_REFERENCE_ID})})
public class CategoryReference extends GdnBaseEntity {

  private static final long serialVersionUID = 4739975309956227968L;

  public static final String TABLE_NAME = "PCC_CATEGORY_REFERENCE";
  public static final String COLUMN_MASTER_CATEGORY_REFERENCE_ID = "MASTER_CATEGORY_REFERENCE_ID";
  public static final String COLUMN_SALES_CATEGORY_REFERENCE_ID = "SALES_CATEGORY_REFERENCE_ID";

  @ManyToOne
  @JoinColumn(name = CategoryReference.COLUMN_MASTER_CATEGORY_REFERENCE_ID)
  private Category masterCategory;

  @Column(name = CategoryReference.COLUMN_MASTER_CATEGORY_REFERENCE_ID, insertable = false, updatable = false)
  private String masterCategoryReferenceId;

  @ManyToOne
  @JoinColumn(name = CategoryReference.COLUMN_SALES_CATEGORY_REFERENCE_ID)
  private Category salesCategory;

  @Column(name = CategoryReference.COLUMN_SALES_CATEGORY_REFERENCE_ID, insertable = false, updatable = false)
  private String salesCategoryReferenceId;

  public CategoryReference() {}

  public CategoryReference(Category masterCategory, Category salesCategory) {
    this.masterCategory = masterCategory;
    this.salesCategory = salesCategory;
  }

  public Category getMasterCategory() {
    return this.masterCategory;
  }

  public Category getSalesCategory() {
    return this.salesCategory;
  }

  public void setMasterCategory(Category masterCategory) {
    this.masterCategory = masterCategory;
  }

  public void setSalesCategory(Category salesCategory) {
    this.salesCategory = salesCategory;
  }

  @Override
  public String toString() {
    return "CategoryReference{" + "masterCategory=" + this.masterCategory + ", salesCategory=" + this.salesCategory
        + '}';
  }
}
