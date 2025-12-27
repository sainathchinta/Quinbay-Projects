package com.gdn.x.product.rest.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class HalalProductResponse extends BaseResponse {
  private static final long serialVersionUID = 5080170068657165324L;
  private String productSku;
  private String productName;
  private String productCode;
  private String brand;
  private boolean halalProduct;
  private String curationStatus;
  private Date productCreationDate;
}
