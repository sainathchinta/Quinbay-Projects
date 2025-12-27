package com.gdn.x.product.rest.web.model.response;

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
public class ProductSkuSummaryResponse extends BaseResponse {
  String productCode;
  String productSku;
  String productName;
  String productImage;
  String categoryCode;
  String brand;
  boolean isArchived;
}
