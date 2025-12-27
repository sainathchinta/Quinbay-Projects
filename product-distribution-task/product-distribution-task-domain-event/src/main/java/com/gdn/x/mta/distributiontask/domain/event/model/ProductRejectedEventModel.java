package com.gdn.x.mta.distributiontask.domain.event.model;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductRejectedEventModel extends GdnBaseDomainEventModel {
  private String productCode;
  private Date rejectedDate;

  public ProductRejectedEventModel() {}

  public ProductRejectedEventModel(String productCode, Date rejectedDate) {
    this.productCode = productCode;
    this.rejectedDate = rejectedDate;
  }

  public String getProductCode() {
    return productCode;
  }

  public Date getRejectedDate() {
    return rejectedDate;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setRejectedDate(Date rejectedDate) {
    this.rejectedDate = rejectedDate;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductRejectedEventModel [productCode=");
    builder.append(productCode);
    builder.append(", rejectedDate=");
    builder.append(rejectedDate);
    builder.append("]");
    return builder.toString();
  }
}
