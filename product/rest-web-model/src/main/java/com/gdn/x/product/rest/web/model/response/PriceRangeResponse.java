package com.gdn.x.product.rest.web.model.response;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PriceRangeResponse extends BaseResponse {
  private String webSku;
  private String skuName;
  private String priceRange;
}
