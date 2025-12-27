package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;

public class ProductApprovalDetailStatusEvent extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 2032789675510393257L;
  private String productCode;
  private ProductApprovalDetailStatus status;
  private long statusTimeStamp;

  public ProductApprovalDetailStatusEvent(String productCode, ProductApprovalDetailStatus status, long statusTimeStamp) {
    this.productCode = productCode;
    this.status = status;
    this.statusTimeStamp = statusTimeStamp;
  }

  public ProductApprovalDetailStatusEvent(String productCode, ProductApprovalDetailStatus status) {
    this.productCode = productCode;
    this.status = status;
    this.statusTimeStamp = System.currentTimeMillis();
  }

  public ProductApprovalDetailStatusEvent() {
  }

  public long getStatusTimeStamp() {
    return statusTimeStamp;
  }

  public void setStatusTimeStamp(long statusTimeStamp) {
    this.statusTimeStamp = statusTimeStamp;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public ProductApprovalDetailStatus getStatus() {
    return status;
  }

  public void setStatus(ProductApprovalDetailStatus status) {
    this.status = status;
  }

  @Override
  public String toString() {
    return "ProductApprovalDetailStatusEvent{" + "productCode='" + productCode + '\'' + ", status=" + status
        + ", statusTimeStamp=" + statusTimeStamp + '}';
  }
}
