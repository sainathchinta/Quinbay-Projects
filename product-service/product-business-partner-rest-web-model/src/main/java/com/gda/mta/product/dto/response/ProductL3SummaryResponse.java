package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
public class ProductL3SummaryResponse extends BaseResponse {

  String productSku;
  String productSkuName;
  String productImage;
  String categoryName;
  String categoryCode;
  String categoryHierarchy;
  String brand;
  boolean isArchived;
}
