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
@Table(name = MerchantConfigurationHistory.TABLE_NAME)
public class MerchantConfigurationHistory extends GdnBaseEntity {

  private static final long serialVersionUID = -3081430072987423461L;

  public static final String TABLE_NAME = "PCC_MERCHANT_CONFIGURATION_HISTORY";
  private static final String COLUMN_MERCHANT_CODE = "MERCHANT_CODE";
  private static final String COLUMN_MERCHANT_NAME = "MERCHANT_NAME";
  private static final String COLUMN_OLD_VALUE = "OLD_VALUE";
  private static final String COLUMN_NEW_VALUE = "NEW_VALUE";
  private static final String COLUMN_ACTIVITY = "ACTIVITY";

  @Column(name = MerchantConfigurationHistory.COLUMN_MERCHANT_CODE)
  private String merchantCode;

  @Column(name = MerchantConfigurationHistory.COLUMN_MERCHANT_NAME)
  private String merchantName;

  @Column(name = MerchantConfigurationHistory.COLUMN_OLD_VALUE)
  private String oldValue;

  @Column(name = MerchantConfigurationHistory.COLUMN_NEW_VALUE)
  private String newValue;

  @Column(name = MerchantConfigurationHistory.COLUMN_ACTIVITY)
  private String activity;
}
