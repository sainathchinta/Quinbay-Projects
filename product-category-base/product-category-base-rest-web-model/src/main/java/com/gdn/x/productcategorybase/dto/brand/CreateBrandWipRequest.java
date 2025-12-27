package com.gdn.x.productcategorybase.dto.brand;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CreateBrandWipRequest implements Serializable {

  private static final long serialVersionUID = -5957915252876803431L;
  private static final String NOT_AVAILABLE = "N/A";

  private String brandName;
  private String brandDescription;
  private String brandLogoPath = NOT_AVAILABLE;
  private String profileBannerPath = NOT_AVAILABLE;
  private String businessPartnerCode;
  private String businessPartnerName;
  private boolean protectedBrand;
  private boolean skuCreationAllowedForAllSellers;
}
