package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper=true)
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = CategoryConfigurationHistory.TABLE_NAME)
public class CategoryConfigurationHistory extends GdnBaseEntity {

  private static final long serialVersionUID = 1742921307215729073L;

  public static final String TABLE_NAME = "PCC_CATEGORY_CONFIGURATION_HISTORY";
  private static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  private static final String COLUMN_CATEGORY_NAME = "CATEGORY_NAME";
  private static final String COLUMN_OLD_VALUE = "OLD_VALUE";
  private static final String COLUMN_NEW_VALUE = "NEW_VALUE";
  private static final String COLUMN_ACTIVITY = "ACTIVITY";

  @Column(name = CategoryConfigurationHistory.COLUMN_CATEGORY_CODE)
  private String categoryCode;

  @Column(name = CategoryConfigurationHistory.COLUMN_CATEGORY_NAME)
  private String categoryName;

  @Column(name = CategoryConfigurationHistory.COLUMN_OLD_VALUE)
  private String oldValue;

  @Column(name = CategoryConfigurationHistory.COLUMN_NEW_VALUE)
  private String newValue;

  @Column(name = CategoryConfigurationHistory.COLUMN_ACTIVITY)
  private String activity;
}
