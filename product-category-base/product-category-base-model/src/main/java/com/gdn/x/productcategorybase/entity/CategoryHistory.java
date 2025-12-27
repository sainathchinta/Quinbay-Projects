package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper = true)
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = CategoryHistory.TABLE_NAME)
public class CategoryHistory extends GdnBaseEntity {

  public static final String TABLE_NAME = "PCC_CATEGORY_HISTORY";
  private static final long serialVersionUID = 1138970374861735095L;
  private static final String COLUMN_ACTIVITY = "ACTIVITY";
  private static final String COLUMN_OLD_STATUS = "OLD_STATUS";
  private static final String COLUMN_NEW_STATUS = "NEW_STATUS";
  private static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";

  @Column(name = COLUMN_ACTIVITY)
  private String activity;

  @Column(name = COLUMN_OLD_STATUS)
  private String oldStatus;

  @Column(name = COLUMN_NEW_STATUS)
  private String newStatus;

  @Column(name = COLUMN_CATEGORY_CODE, nullable = false)
  private String categoryCode;

}
