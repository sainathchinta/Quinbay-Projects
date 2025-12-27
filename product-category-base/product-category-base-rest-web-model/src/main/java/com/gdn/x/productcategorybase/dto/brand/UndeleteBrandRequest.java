package com.gdn.x.productcategorybase.dto.brand;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UndeleteBrandRequest implements Serializable {

  private static final long serialVersionUID = 2538431637116002905L;
  private String brandName;
  private String brandDescription;
  private String brandLogoPath;
  private String profileBannerPath;

  public UndeleteBrandRequest() {}

  public UndeleteBrandRequest(String brandName, String brandDescription, String brandLogoPath) {
    super();
    this.brandName = brandName;
    this.brandDescription = brandDescription;
    this.brandLogoPath = brandLogoPath;
  }

  public UndeleteBrandRequest(String brandName, String brandDescription, String brandLogoPath,
      String profileBannerPath) {
    this(brandName, brandDescription, brandLogoPath);
    this.profileBannerPath = profileBannerPath;
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

  @Override
  public String toString() {
    return String.format("UndeleteBrandRequest [brandName=%s, brandDescription=%s, brandLogoPath=%s, profileBannerPath=%s]",
        brandName, brandDescription, brandLogoPath, profileBannerPath);
  }

}
