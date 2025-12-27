package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import com.gdn.GdnBaseEntity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = ProductSystemParameter.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, ProductSystemParameter.COLUMN_VARIABLE})})
public class ProductSystemParameter extends GdnBaseEntity {

  private static final long serialVersionUID = 4958236247134601549L;
  public static final String TABLE_NAME = "PRD_PRODUCT_SYSTEM_PARAMETER";
  public static final String COLUMN_VARIABLE = "VARIABLE";
  public static final String COLUMN_VALUE = "VALUE";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_SHOW_ON_UI = "SHOW_ON_UI";

  @Column(name = COLUMN_VARIABLE, nullable = false)
  private String variable;

  @Column(name = COLUMN_VALUE, nullable = false)
  private String value;

  @Column(name = COLUMN_DESCRIPTION, nullable = false)
  private String description;

  @Column(name = COLUMN_SHOW_ON_UI)
  private boolean showOnUI;
}
