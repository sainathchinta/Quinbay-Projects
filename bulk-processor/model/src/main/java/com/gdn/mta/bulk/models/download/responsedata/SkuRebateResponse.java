package com.gdn.mta.bulk.models.download.responsedata;

import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SkuRebateResponse extends BaseResponse {

  private static final long serialVersionUID = 2921287518111056576L;

  private Double sellerLevel;
  private Double sellerCategoryLevel;
  private Double sellerCategoryBrandLevel;
  private Double sellerBrandLevel;
}
