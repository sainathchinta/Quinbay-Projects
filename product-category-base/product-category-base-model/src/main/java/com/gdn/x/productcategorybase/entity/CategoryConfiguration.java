package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.builder.ToStringBuilder;


@Data
@EqualsAndHashCode(callSuper=true)
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = CategoryConfiguration.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {CategoryConfiguration.COLUMN_CATEGORY_ID, GdnBaseEntity.STORE_ID})})
public class CategoryConfiguration extends GdnBaseEntity {

  private static final long serialVersionUID = -3309651783631469621L;

  public static final String TABLE_NAME = "PCC_CATEGORY_CONFIGURATION";
  public static final String COLUMN_CATEGORY_ID = "CATEGORY_ID";
  private static final String COLUMN_REVIEW_CONFIG = "REVIEW_CONFIG";

  @OneToOne
  @JoinColumn(name = CategoryConfiguration.COLUMN_CATEGORY_ID)
  private Category category;

  @Column(name = CategoryConfiguration.COLUMN_CATEGORY_ID, insertable = false, updatable = false)
  private String categoryId;

  @Column(name = CategoryConfiguration.COLUMN_REVIEW_CONFIG)
  private String reviewConfig;

  public CategoryConfiguration(Category category, String reviewConfig) {
    this.category = category;
    this.reviewConfig = reviewConfig;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).
        append("category", category).
        append("reviewConfig", reviewConfig).
        toString();
  }
}
