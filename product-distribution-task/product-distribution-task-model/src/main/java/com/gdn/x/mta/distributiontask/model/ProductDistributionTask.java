package com.gdn.x.mta.distributiontask.model;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

@Table(name = "PDT_PRODUCT_DISTRIBUTION_TASK")
@Entity
public class ProductDistributionTask extends GdnBaseEntity {
  private static final long serialVersionUID = 1L;

  @Column(name = "TASK_CODE")
  private String taskCode;

  @JoinColumn(name = "VENDOR")
  @ManyToOne
  private Vendor vendor;

  @Column(name = "VENDOR", insertable = false, updatable = false)
  private String vendorId;

  @JoinColumn(name = "PRODUCT")
  @ManyToOne
  private Product product;

  @Column(name = "PRODUCT", insertable = false, updatable = false)
  private String productId;

  @Column(name = "REJECTED_COUNT")
  private Integer rejectedCount;

  @Column(name = "STATE")
  @Enumerated(EnumType.STRING)
  private WorkflowState state;

  @Column(name = "SLA_DATE")
  @Temporal(TemporalType.TIMESTAMP)
  private Date slaDate;

  @Column(name = "IS_SLA_DATE_EXCEED")
  private boolean slaDateExceed;

  public ProductDistributionTask() {}

  public ProductDistributionTask(String taskCode, Vendor vendor, Product product,
      WorkflowState state, Date slaDate) {
    this.taskCode = taskCode;
    this.vendor = vendor;
    this.product = product;
    this.state = state;
    this.slaDate = slaDate;
  }

  public Product getProduct() {
    return product;
  }

  public String getProductId() {
    return productId;
  }

  public Integer getRejectedCount() {
    return rejectedCount;
  }

  public Date getSlaDate() {
    return slaDate;
  }

  public WorkflowState getState() {
    return state;
  }

  public String getTaskCode() {
    return taskCode;
  }

  public Vendor getVendor() {
    return vendor;
  }

  public String getVendorId() {
    return vendorId;
  }

  public boolean isSlaDateExceed() {
    return slaDateExceed;
  }

  public void setProduct(Product product) {
    this.product = product;
  }

  public void setProductId(String productId) {
    this.productId = productId;
  }

  public void setRejectedCount(Integer rejectedCount) {
    this.rejectedCount = rejectedCount;
  }

  public void setSlaDate(Date slaDate) {
    this.slaDate = slaDate;
  }

  public void setSlaDateExceed(boolean slaDateExceed) {
    this.slaDateExceed = slaDateExceed;
  }

  public void setState(WorkflowState state) {
    this.state = state;
  }

  public void setTaskCode(String taskCode) {
    this.taskCode = taskCode;
  }

  public void setVendor(Vendor vendor) {
    this.vendor = vendor;
  }

  public void setVendorId(String vendorId) {
    this.vendorId = vendorId;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductDistributionTask{");
    sb.append("taskCode='").append(taskCode).append('\'');
    sb.append(", vendor=").append(vendor);
    sb.append(", vendorId='").append(vendorId).append('\'');
    sb.append(", product=").append(product);
    sb.append(", productId='").append(productId).append('\'');
    sb.append(", rejectedCount=").append(rejectedCount).append('\'');
    sb.append(", slaDate=").append(slaDate).append('\'');
    sb.append(", state=").append(state);
    sb.append('}');
    return sb.toString();
  }
}
