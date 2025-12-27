package com.gdn.x.product.rest.web.model.response;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ReelProductDetailResponse extends BaseResponse {
  private String productSku;
  private String productName;
  private String mainImageUrl;
  private Double minimumSellingPrice;
  private Double maximumSellingPrice;
  private Boolean inStock;
  private Boolean archived;
  private Boolean suspended;
}
