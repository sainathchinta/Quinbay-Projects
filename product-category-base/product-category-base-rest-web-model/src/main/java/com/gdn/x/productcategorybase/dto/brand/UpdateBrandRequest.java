package com.gdn.x.productcategorybase.dto.brand;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateBrandRequest implements Serializable {

  private static final long serialVersionUID = -924213274025779380L;
  private String brandCode;
  private String brandName;
  private String brandDescription;
  private String brandLogoPath;
  private String profileBannerPath;
  private String brandRequestCode;
  private String brandLogo;
  private String profileBanner;
  private boolean validBrand;
  private boolean protectedBrand;
  private Boolean skuCreationAllowedForAllSellers;

  public UpdateBrandRequest(String brandCode, String brandName, String brandDescription, String brandLogoPath) {
    super();
    this.brandCode = brandCode;
    this.brandName = brandName;
    this.brandDescription = brandDescription;
    this.brandLogoPath = brandLogoPath;
  }

  public UpdateBrandRequest(String brandCode, String brandName, String brandDescription,
      String brandLogoPath, String profileBannerPath) {
    this(brandCode, brandName, brandDescription, brandLogoPath);
    this.profileBannerPath = profileBannerPath;
  }

  public UpdateBrandRequest(String brandCode, String brandName, String brandDescription, String brandLogoPath,
      String profileBannerPath, String brandRequestCode, String brandLogo, String profileBanner) {
    this(brandCode, brandName, brandDescription, brandLogoPath, profileBannerPath);
    this.brandRequestCode = brandRequestCode;
    this.brandLogo = brandLogo;
    this.profileBanner = profileBanner;
  }
}
