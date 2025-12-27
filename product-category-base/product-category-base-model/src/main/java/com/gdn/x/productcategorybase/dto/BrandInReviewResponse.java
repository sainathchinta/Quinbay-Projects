package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandInReviewResponse extends BaseResponse {

  public String brandName;
  public String brandCode;
  public String businessPartnerCode;
  public boolean protectedBrand;
  public String brandRequestCode;
  public BrandWipState state;
}

