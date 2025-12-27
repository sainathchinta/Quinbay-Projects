package com.gdn.partners.product.orchestrator.dto.product.level1;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel1FilterResponse extends ProductDetailResponse {

  private String productSku;
  private String state;
  private String rejectionNotes;
  private String revisionNotes;
  private boolean isPreOrder;
}
