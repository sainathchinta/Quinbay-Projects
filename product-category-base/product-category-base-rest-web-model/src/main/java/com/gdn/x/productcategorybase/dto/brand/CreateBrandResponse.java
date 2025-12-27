package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CreateBrandResponse extends BaseResponse {

  private static final long serialVersionUID = 884922789868480536L;
  private String brandCode;
  private String brandName;
  private String brandLogoPath;
  private String profileBannerPath;

  public CreateBrandResponse() {}

  public CreateBrandResponse(String brandCode) {
    super();
    this.brandCode = brandCode;
  }

  public CreateBrandResponse(String brandCode, String brandLogoPath, String profileBannerPath, String brandName) {
    super();
    this.brandCode = brandCode;
    this.brandLogoPath = brandLogoPath;
    this.profileBannerPath= profileBannerPath;
    this.brandName = brandName;
  }

  public CreateBrandResponse(String brandCode, String brandLogoPath, String profileBannerPath) {
    super();
    this.brandCode = brandCode;
    this.brandLogoPath = brandLogoPath;
    this.profileBannerPath= profileBannerPath;
  }

  public String getBrandCode() {
    return brandCode;
  }

  public void setBrandCode(String brandCode) {
    this.brandCode = brandCode;
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

  public String getBrandName() {
    return brandName;
  }

  public void setBrandName(String brandName) {
    this.brandName = brandName;
  }

  @Override
  public String toString() {
    return String.format("CreateBrandResponse [brandCode=%s]", brandCode);
  }

}
