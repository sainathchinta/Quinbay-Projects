package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ApproveBrandWipWebRequest {

  private String brandRequestCode;
  private String brandName;
  private String brandDescription;
  private String brandLogoPath;
  private String profileBannerPath;
  private boolean validBrand;
  private boolean protectedBrand;
  private boolean skuCreationAllowedForAllSellers;
}
