package com.gdn.x.product.rest.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class HalalDashboardProductsResponse extends BaseResponse {
  private static final long serialVersionUID = 2233384864266414295L;
  private String brand;
  private String categoryCode;
  private String productCode;
  private String productName;
  private String productSku;
  private Date productCreationDate;
  private String curationStatus;
  private boolean halalProduct;
}
