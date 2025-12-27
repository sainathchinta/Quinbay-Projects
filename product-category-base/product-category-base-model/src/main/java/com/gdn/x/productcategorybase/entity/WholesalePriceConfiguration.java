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
@Table(name = WholesalePriceConfiguration.TABLE_NAME)
public class WholesalePriceConfiguration extends GdnBaseEntity {

  private static final long serialVersionUID = -876934798097396729L;
  public static final String TABLE_NAME = "PCC_CATEGORY_WHOLESALE_PRICE_CONFIGURATION";
  public static final String COLUMN_CATEGORY_ID = "CATEGORY_ID";
  public static final String COLUMN_CONFIGURATION_TYPE = "CONFIGURATION_TYPE";
  public static final String COLUMN_WHOLESALE_CONFIG = "WHOLESALE_CONFIG";

  @Column(name = WholesalePriceConfiguration.COLUMN_CATEGORY_ID)
  private String categoryId;

  @Column(name = WholesalePriceConfiguration.COLUMN_CONFIGURATION_TYPE)
  private String configurationType;

  @Column(name = WholesalePriceConfiguration.COLUMN_WHOLESALE_CONFIG)
  private String wholesaleConfigs;
}
