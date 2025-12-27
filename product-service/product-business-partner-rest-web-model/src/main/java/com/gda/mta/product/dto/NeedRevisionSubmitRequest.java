package com.gda.mta.product.dto;

import java.io.Serializable;

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
public class NeedRevisionSubmitRequest extends BaseRequest implements Serializable {

  private static final long serialVersionUID = -1050054718617746726L;
  private String productCode;
  private String productSku;
  private boolean appealedProduct;
  private String appealedProductNotes;

  public NeedRevisionSubmitRequest(String productCode, String productSku) {
    this.productCode = productCode;
    this.productSku = productSku;
  }
}
