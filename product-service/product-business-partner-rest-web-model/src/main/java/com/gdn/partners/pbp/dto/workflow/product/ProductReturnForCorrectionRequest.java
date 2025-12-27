package com.gdn.partners.pbp.dto.workflow.product;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductReturnForCorrectionRequest implements Serializable {

  private static final long serialVersionUID = -7067594599284557723L;

  private String productCode;
  private String notes;

  public ProductReturnForCorrectionRequest() {
    super();
  }

  public ProductReturnForCorrectionRequest(String productCode, String notes) {
    super();
    this.productCode = productCode;
    this.notes = notes;
  }

  public String getNotes() {
    return notes;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  @Override
  public String toString() {
    return "ProductReturnForCorrectionRequest [productCode=" + productCode + ", notes=" + notes
        + "]";
  }
}
