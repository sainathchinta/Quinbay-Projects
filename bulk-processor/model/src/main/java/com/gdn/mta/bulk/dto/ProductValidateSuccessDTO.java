package com.gdn.mta.bulk.dto;

import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;

/**
 * Created by virajjasani on 03/08/16.
 */
public class ProductValidateSuccessDTO {

  private String gdnSku;
  private ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest;

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public ProductLevel3UpdateSummaryRequest getProductLevel3UpdateSummaryRequest() {
    return productLevel3UpdateSummaryRequest;
  }

  public void setProductLevel3UpdateSummaryRequest(
      ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest) {
    this.productLevel3UpdateSummaryRequest = productLevel3UpdateSummaryRequest;
  }

  public ProductValidateSuccessDTO(String gdnSku,
      ProductLevel3UpdateSummaryRequest productLevel3UpdateSummaryRequest) {
    this.gdnSku = gdnSku;
    this.productLevel3UpdateSummaryRequest = productLevel3UpdateSummaryRequest;
  }

  public ProductValidateSuccessDTO() {
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductValidateSuccessDTO{");
    sb.append("gdnSku='").append(gdnSku).append('\'');
    sb.append(", productLevel3UpdateSummaryRequest=").append(productLevel3UpdateSummaryRequest);
    sb.append('}');
    return sb.toString();
  }
}
