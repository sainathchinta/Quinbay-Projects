package com.gdn.x.productcategorybase.dto.brand;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class BrandWipResponse extends BaseResponse {
  private static final long serialVersionUID = -5012777425752200304L;
  private String id;
  private String brandCode;
  private String brandName;
  private String brandDescription;
  private String brandLogoPath;
  private String profileBannerPath;
  private String state;
  private String brandRequestCode;
  private String businessPartnerName;
  private String businessPartnerCode;
  private boolean validBrand;
  private boolean protectedBrand;
  private boolean skuCreationAllowedForAllSellers;
}
