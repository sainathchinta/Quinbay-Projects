package com.gdn.x.mta.distributiontask.response;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public class StuckProductsResponse implements Serializable {
  private static final long serialVersionUID = 5788687194476282829L;
  private String productCode;
  private String state;
  private String createdDate;
  private String updatedDate;

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

  public String getCreatedDate() {
    return createdDate;
  }

  public void setCreatedDate(String createdDate) {
    this.createdDate = createdDate;
  }

  public String getUpdatedDate() {
    return updatedDate;
  }

  public void setUpdatedDate(String updatedDate) {
    this.updatedDate = updatedDate;
  }

  @Override
  public String toString() {
    return "StuckProductsResponse{" + "productCode='" + productCode + '\'' + ", state='" + state
        + '\'' + ", createdDate='" + createdDate + '\'' + ", updatedDate='" + updatedDate + '\''
        + '}';
  }
}
