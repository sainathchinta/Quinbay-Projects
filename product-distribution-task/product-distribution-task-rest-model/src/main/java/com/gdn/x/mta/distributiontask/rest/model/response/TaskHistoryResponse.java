package com.gdn.x.mta.distributiontask.rest.model.response;

import com.gdn.common.web.base.BaseResponse;

public class TaskHistoryResponse extends BaseResponse {
  private static final long serialVersionUID = -3192216689947017910L;
  private String productCode;
  private String productName;
  private String categoryCode;
  private String categoryName;
  private String vendorName;
  private String vendorCode;
  private String taskCode;
  private String reason;
  private String state;

  public TaskHistoryResponse() {}

  public TaskHistoryResponse(String productCode, String productName, String categoryCode,
      String vendorName, String vendorCode, String reason, String state, String taskCode) {
    super();
    this.productCode = productCode;
    this.productName = productName;
    this.categoryCode = categoryCode;
    this.vendorName = vendorName;
    this.vendorCode = vendorCode;
    this.reason = reason;
    this.state = state;
    this.taskCode = taskCode;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getProductName() {
    return productName;
  }

  public String getReason() {
    return reason;
  }

  public String getState() {
    return state;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public String getVendorName() {
    return vendorName;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setReason(String reason) {
    this.reason = reason;
  }

  public void setState(String state) {
    this.state = state;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public void setVendorName(String vendorName) {
    this.vendorName = vendorName;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("TaskHistoryResponse [productCode=");
    builder.append(productCode);
    builder.append(", productName=");
    builder.append(productName);
    builder.append(", categoryCode=");
    builder.append(categoryCode);
    builder.append(", categoryName=");
    builder.append(categoryName);
    builder.append(", vendorName=");
    builder.append(vendorName);
    builder.append(", vendorCode=");
    builder.append(vendorCode);
    builder.append(", taskCode=");
    builder.append(taskCode);
    builder.append(", reason=");
    builder.append(reason);
    builder.append(", state=");
    builder.append(state);
    builder.append("]");
    return builder.toString();
  }
}
