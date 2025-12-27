package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.enums.ProductType;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
@ToString
public class ProductTypeEditRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private ProductType productType;
  private String productCode;
  private String productSku;
  private boolean contentChanged;
}
