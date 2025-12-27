package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

@Table(name = "PDT_DISTRIBUTION_TASK_HISTORY")
@Entity
public class TaskHistory extends GdnBaseEntity {

  private static final long serialVersionUID = 1L;

  @Column(name = "PRODUCT_CODE")
  private String productCode;

  @Column(name = "PRODUCT_NAME")
  private String productName;

  @Column(name = "CATEGORY_CODE")
  private String categoryCode;

  @Column(name = "CATEGORY_NAME")
  private String categoryName;

  @JoinColumn(name = "VENDOR")
  @ManyToOne
  private Vendor vendor;

  @Column(name = "REASON")
  private String reason;

  @Column(name = "TASK_CODE")
  private String taskCode;

  @Column(name = "STATE")
  @Enumerated(EnumType.STRING)
  private WorkflowState state;

  public TaskHistory() {}

  public TaskHistory(String productCode, String productName, String categoryCode, String categoryName, Vendor vendor, String reason,
      WorkflowState state, String storeId, String executor, String taskCode) {
    super();
    this.productCode = productCode;
    this.productName = productName;
    this.categoryCode = categoryCode;
    this.vendor = vendor;
    this.reason = reason;
    this.state = state;
    this.categoryName = categoryName;
    this.setStoreId(storeId);
    this.setCreatedBy(executor);
    this.taskCode = taskCode;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public Vendor getVendor() {
    return vendor;
  }

  public void setVendor(Vendor vendor) {
    this.vendor = vendor;
  }

  public String getReason() {
    return reason;
  }

  public void setReason(String reason) {
    this.reason = reason;
  }

  public WorkflowState getState() {
    return state;
  }

  public void setState(WorkflowState state) {
    this.state = state;
  }

  public String getTaskCode() {
    return taskCode;
  }

  public void setTaskCode(String taskCode) {
    this.taskCode = taskCode;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("TaskHistory [productCode=");
    builder.append(productCode);
    builder.append(", productName=");
    builder.append(productName);
    builder.append(", categoryCode=");
    builder.append(categoryCode);
    builder.append(", categoryName=");
    builder.append(categoryName);
    builder.append(", vendor=");
    builder.append(vendor);
    builder.append(", reason=");
    builder.append(reason);
    builder.append(", taskCode=");
    builder.append(taskCode);
    builder.append(", state=");
    builder.append(state);
    builder.append("]");
    return builder.toString();
  }

}
