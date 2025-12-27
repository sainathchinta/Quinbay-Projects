package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ProductAndItemsSummaryResponseV2 extends BaseResponse {
  private static final long serialVersionUID = -8266576885321087622L;
  private ProductDetailResponseV2 productDetailResponseV2;
  private List<ItemResponse> itemDetailResponsesV2;
}