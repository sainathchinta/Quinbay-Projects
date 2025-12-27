package com.gda.mta.product.dto.generator;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductWfStateResponse extends BaseResponse {
  private static final long serialVersionUID = -1461343352728751089L;
  private String productCode;
  private String state;

  public ProductWfStateResponse() {
    super();
  }

  public ProductWfStateResponse(String productCode, String state) {
    this.productCode = productCode;
    this.state = state;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getState() {
    return state;
  }

  public void setState(String state) {
    this.state = state;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductWfStateResponse{");
    sb.append("productCode='").append(productCode).append('\'');
    sb.append(", state='").append(state).append('\'');
    sb.append('}');
    return sb.toString();
  }
}