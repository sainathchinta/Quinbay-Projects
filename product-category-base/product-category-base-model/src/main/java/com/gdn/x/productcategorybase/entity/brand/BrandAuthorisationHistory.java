package com.gdn.x.productcategorybase.entity.brand;

import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Entity;

@Data
@EqualsAndHashCode(callSuper=true)
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = BrandAuthorisationHistory.TABLE_NAME)
public class BrandAuthorisationHistory extends GdnBaseEntity {

  private static final long serialVersionUID = 6514347506121084108L;
  public static final String TABLE_NAME = "PCC_BRAND_AUTHORISATION_HISTORY";
  private static final String COLUMN_ACTIVITY = "ACTIVITY";
  private static final String COLUMN_OLD_STATUS = "OLD_STATUS";
  private static final String COLUMN_NEW_STATUS = "NEW_STATUS";
  private static final String COLUMN_BRAND_CODE = "BRAND_CODE";
  private static final String COLUMN_SELLER_CODE = "SELLER_CODE";

  @Column(name = COLUMN_ACTIVITY)
  private String activity;

  @Column(name = COLUMN_OLD_STATUS)
  private String oldStatus;

  @Column(name = COLUMN_NEW_STATUS)
  private String newStatus;

  @Column(name = COLUMN_BRAND_CODE)
  private String brandCode;

  @Column(name = COLUMN_SELLER_CODE)
  private String sellerCode;

}