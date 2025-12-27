package com.gdn.partners.pcu.external.web.model.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class NeedRevisionSubmitWebRequest {
  private String productCode;
  private String productSku;
  private boolean appealedProduct;
  private String appealedProductNotes;

  public NeedRevisionSubmitWebRequest(String productCode, String productSku) {
    this.productCode = productCode;
    this.productSku = productSku;
  }
}
