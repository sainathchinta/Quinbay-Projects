package com.gdn.x.productcategorybase.entity.brand;

import com.gdn.x.productcategorybase.constants.FieldNames;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper=true)
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = BrandWip.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(
      columnNames = {FieldNames.COLUMN_BRAND_REQUEST_CODE})})
public class BrandWip extends GdnBaseEntity {

  private static final long serialVersionUID = -2425041595721109219L;

  public static final String TABLE_NAME = "PCC_BRAND_WIP";

  @Column(name = FieldNames.COLUMN_BRAND_REQUEST_CODE, nullable = false)
  private String brandRequestCode;

  @Column(name = FieldNames.COLUMN_BRAND_CODE)
  private String brandCode;

  @Column(name = FieldNames.COLUMN_BRAND_NAME, nullable = false)
  private String brandName;

  @Column(name = FieldNames.COLUMN_BRAND_DESCRIPTION, nullable = false)
  private byte[] brandDescription;

  @Column(name = FieldNames.COLUMN_BRAND_LOGO_PATH)
  private String brandLogoPath;

  @Column(name = FieldNames.COLUMN_BUSINESS_PARTNER_CODE, nullable = false)
  private String businessPartnerCode;

  @Column(name = FieldNames.COLUMN_BUSINESS_PARTNER_NAME, nullable = false)
  private String businessPartnerName;

  @Enumerated(value = EnumType.STRING)
  @Column(name = FieldNames.COLUMN_STATE, nullable = false)
  private BrandWipState state;

  @Column(name = FieldNames.COLUMN_NOTES)
  private byte[] notes;

  @Column(name = FieldNames.COLUMN_PROFILE_BANNER_PATH)
  private String profileBannerPath;

  @Column(name = FieldNames.COLUMN_VALID_BRAND)
  private boolean validBrand;

  @Column(name = FieldNames.COLUMN_PROTECTED_BRAND)
  private boolean protectedBrand;

  @Column(name = FieldNames.COLUMN_SKU_CREATION_ALLOWED_FOR_ALL_SELLERS)
  private boolean skuCreationAllowedForAllSellers;
}
