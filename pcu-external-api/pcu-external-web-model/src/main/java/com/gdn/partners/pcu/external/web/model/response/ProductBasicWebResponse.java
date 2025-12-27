package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBasicWebResponse extends BaseResponse {
  private boolean productExists;
  private String productSku;
  private String productName;
  private boolean suspended;
  private boolean archived;
  private boolean markForDelete;
  private boolean tradingProduct;
  private boolean bundleProduct;
  private String categoryCode;
  private ProductType productType;
}
