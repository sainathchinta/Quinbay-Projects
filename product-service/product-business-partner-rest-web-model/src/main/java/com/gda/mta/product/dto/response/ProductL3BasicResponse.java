package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL3BasicResponse  extends BaseResponse {

  private String productCode;
  private String productSku;
  private boolean b2cActivated;
  private boolean off2OnChannelActive;
  private boolean pureInStoreProduct;
}
