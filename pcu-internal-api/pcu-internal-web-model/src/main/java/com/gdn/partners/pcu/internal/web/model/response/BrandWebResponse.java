package com.gdn.partners.pcu.internal.web.model.response;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandWebResponse {

  private String id;
  private String brandCode;
  private String brandName;
  private String brandDescription;
  private String brandLogoPath;
  private String profileBannerPath;
  private boolean validBrand;
  private boolean protectedBrand;
  private boolean skuCreationAllowedForAllSellers;
}
