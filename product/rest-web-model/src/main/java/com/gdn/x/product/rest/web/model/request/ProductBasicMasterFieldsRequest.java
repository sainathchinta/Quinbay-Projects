package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
public class ProductBasicMasterFieldsRequest implements Serializable {

  private static final long serialVersionUID= -7160919271662044165L;
  private String productSku;
  private boolean instore;
  private ProductType productType;
  private String sizeChartCode;
  private boolean generateProductScoreNeeded;
  private Boolean lateFulfillment;
}
