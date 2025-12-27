package com.gdn.x.productcategorybase.entity.brand;

import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = Brand.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, Brand.COLUMN_BRAND_CODE}),
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, Brand.COLUMN_BRAND_NAME})})
@EqualsAndHashCode(callSuper=true)
public class Brand extends GdnBaseEntity {

  private static final long serialVersionUID = 5497035264496162370L;
  public static final String TABLE_NAME = "PCC_BRAND";
  public static final String COLUMN_BRAND_CODE = "BRAND_CODE";
  public static final String COLUMN_BRAND_NAME = "BRAND_NAME";
  public static final String COLUMN_BRAND_DESCRIPTION = "BRAND_DESCRIPTION";
  public static final String COLUMN_BRAND_LOGO_PATH = "BRAND_LOGO_PATH";
  public static final String COLUMN_BRAND_INFO = "BRAND_INFO";
  public static final String COLUMN_PROFILE_BANNER_PATH = "PROFILE_BANNER_PATH";
  public static final String COLUMN_BRAND_WIP_ID = "BRAND_WIP_ID";
  public static final String COLUMN_VALID_BRAND = "VALID_BRAND";
  public static final String COLUMN_PROTECTED_BRAND = "PROTECTED_BRAND";
  public static final String COLUMN_SKU_CREATION_ALLOWED_FOR_ALL_SELLERS = "SKU_CREATION_ALLOWED_FOR_ALL_SELLERS";

  @Column(name = Brand.COLUMN_BRAND_CODE, nullable = false)
  private String brandCode;

  @Column(name = Brand.COLUMN_BRAND_NAME, nullable = false)
  private String brandName;

  @Column(name = Brand.COLUMN_BRAND_DESCRIPTION, nullable = false)
  private byte[] brandDescription;

  @Column(name = Brand.COLUMN_BRAND_LOGO_PATH)
  private String brandLogoPath;

  @Column(name = Brand.COLUMN_PROFILE_BANNER_PATH)
  private String profileBannerPath;

  @Column(name = Brand.COLUMN_BRAND_WIP_ID)
  private String brandWipId;

  @Column(name = Brand.COLUMN_VALID_BRAND)
  private boolean validBrand;

  @Column(name = Brand.COLUMN_PROTECTED_BRAND)
  private boolean protectedBrand;

  @Column(name = Brand.COLUMN_SKU_CREATION_ALLOWED_FOR_ALL_SELLERS)
  private boolean skuCreationAllowedForAllSellers;

  public Brand(String brandCode, String brandName, byte[] brandDescription, String brandLogoPath) {
    super();
    this.brandCode = brandCode;
    this.brandName = brandName;
    this.brandDescription = brandDescription;
    this.brandLogoPath = brandLogoPath;
  }

  public Brand(String brandCode, String brandName, byte[] brandDescription, String brandLogoPath,
      String profileBannerPath) {
    this(brandCode, brandName, brandDescription, brandLogoPath);
    this.profileBannerPath = profileBannerPath;
  }
}
