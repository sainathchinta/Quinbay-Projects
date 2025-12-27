package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class InProgressProductResponse extends BaseResponse {

  private static final long serialVersionUID = 4999665731258879964L;

  private String gdnProductItemSku;
  private String productCode;
  private String status;
  private String productSku;

  public InProgressProductResponse(String gdnProductItemSku, String productCode, String status) {
    this.gdnProductItemSku = gdnProductItemSku;
    this.productCode = productCode;
    this.status = status;
  }

  public InProgressProductResponse(String productSku, String status) {
    this.productSku = productSku;
    this.status = status;
  }
}
