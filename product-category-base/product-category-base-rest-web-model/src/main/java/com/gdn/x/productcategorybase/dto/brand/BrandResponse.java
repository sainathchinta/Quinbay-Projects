package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandResponse extends BaseResponse {

  private static final long serialVersionUID = -8752277271277629647L;
  private String brandCode;
  private String brandName;
  private String brandDescription;
  private String brandLogoPath;
  private String profileBannerPath;
  private boolean validBrand;
  private boolean protectedBrand;
  private boolean skuCreationAllowedForAllSellers;

  public BrandResponse() {}

  public BrandResponse(String brandCode, String brandName, String brandDescription, String brandLogoPath) {
    super();
    this.brandCode = brandCode;
    this.brandName = brandName;
    this.brandDescription = brandDescription;
    this.brandLogoPath = brandLogoPath;
  }

  public BrandResponse(String brandCode, String brandName, String brandDescription,
      String brandLogoPath, String profileBannerPath) {
    this(brandCode, brandName, brandDescription, brandLogoPath);
    this.profileBannerPath = profileBannerPath;
  }

  public String getBrandCode() {
    return brandCode;
  }

  public void setBrandCode(String brandCode) {
    this.brandCode = brandCode;
  }

  public String getBrandName() {
    return brandName;
  }

  public void setBrandName(String brandName) {
    this.brandName = brandName;
  }

  public String getBrandDescription() {
    return brandDescription;
  }

  public void setBrandDescription(String brandDescription) {
    this.brandDescription = brandDescription;
  }

  public String getBrandLogoPath() {
    return brandLogoPath;
  }

  public void setBrandLogoPath(String brandLogoPath) {
    this.brandLogoPath = brandLogoPath;
  }

  public String getProfileBannerPath() {
    return profileBannerPath;
  }

  public void setProfileBannerPath(String profileBannerPath) {
    this.profileBannerPath = profileBannerPath;
  }

  public boolean isValidBrand() {
    return validBrand;
  }

  public void setValidBrand(boolean validBrand) {
    this.validBrand = validBrand;
  }

  public boolean isProtectedBrand() {
    return protectedBrand;
  }

  public void setProtectedBrand(boolean protectedBrand) {
    this.protectedBrand = protectedBrand;
  }

  public boolean isSkuCreationAllowedForAllSellers() {
    return skuCreationAllowedForAllSellers;
  }

  public void setSkuCreationAllowedForAllSellers(boolean skuCreationAllowedForAllSellers) {
    this.skuCreationAllowedForAllSellers = skuCreationAllowedForAllSellers;
  }

  @Override
  public String toString() {
    return String.format(
        "BrandResponse [brandCode=%s, brandName=%s, brandDescription=%s, brandLogoPath=%s, "
          + "profileBannerPath=%s, validBrand=%s, protectedBrand=%s, "
          + "skuCreationAllowedForAllSellers=%s]",
        brandCode, brandName, brandDescription, brandLogoPath, profileBannerPath, validBrand,
      protectedBrand, skuCreationAllowedForAllSellers);
  }

}
