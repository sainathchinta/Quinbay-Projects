package com.gdn.x.mta.distributiontask.model.dto;

import java.io.Serializable;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

/**
 * @author febryo.lesmana
 */
public class DistributionTaskFilterDTO implements Serializable {
  public static class Builder {
    private String productName;
    private String categoryCode;
    private String businessPartnerCode;
    private Integer rejected;
    private String vendorCode;
    private WorkflowState status;
    private Integer timeAdded;
    private String storeId;

    public DistributionTaskFilterDTO build() {
      return new DistributionTaskFilterDTO(this);
    }

    public Builder setBusinessPartnerCode(String businessPartnerCode) {
      this.businessPartnerCode = businessPartnerCode;
      return this;
    }

    public Builder setCategoryCode(String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public Builder setRejected(Integer rejected) {
      this.rejected = rejected;
      return this;
    }

    public Builder setStatus(WorkflowState status) {
      this.status = status;
      return this;
    }

    public Builder setStoreId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public Builder setTimeAdded(Integer timeAdded) {
      this.timeAdded = timeAdded;
      return this;
    }

    public Builder setVendorCode(String vendorCode) {
      this.vendorCode = vendorCode;
      return this;
    }

  }

  private static final long serialVersionUID = 1L;
  private String productName;
  private String categoryCode;
  private String businessPartnerCode;
  private Integer rejected;
  private String vendorCode;
  private WorkflowState status;
  private Integer timeAdded;
  private String storeId;

  public DistributionTaskFilterDTO() {}

  protected DistributionTaskFilterDTO(Builder builder) {
    this.productName = builder.productName;
    this.categoryCode = builder.categoryCode;
    this.businessPartnerCode = builder.businessPartnerCode;
    this.rejected = builder.rejected;
    this.vendorCode = builder.vendorCode;
    this.status = builder.status;
    this.timeAdded = builder.timeAdded;
    this.storeId = builder.storeId;
  }

  /**
   * @return the businessPartnerCode
   */
  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  /**
   * @return the categoryCode
   */
  public String getCategoryCode() {
    return categoryCode;
  }

  /**
   * @return the productName
   */
  public String getProductName() {
    return productName;
  }

  /**
   * @return the rejected
   */
  public Integer getRejected() {
    return rejected;
  }

  /**
   * @return the status
   */
  public WorkflowState getStatus() {
    return status;
  }

  public String getStoreId() {
    return storeId;
  }

  /**
   * @return the timeAdded
   */
  public Integer getTimeAdded() {
    return timeAdded;
  }

  /**
   * @return the vendorCode
   */
  public String getVendorCode() {
    return vendorCode;
  }

  /**
   * @param businessPartnerCode the businessPartnerCode to set
   */
  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  /**
   * @param categoryCode the categoryCode to set
   */
  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  /**
   * @param productName the productName to set
   */
  public void setProductName(String productName) {
    this.productName = productName;
  }

  /**
   * @param rejected the rejected to set
   */
  public void setRejected(Integer rejected) {
    this.rejected = rejected;
  }

  /**
   * @param status the status to set
   */
  public void setStatus(WorkflowState status) {
    this.status = status;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  /**
   * @param timeAdded the timeAdded to set
   */
  public void setTimeAdded(Integer timeAdded) {
    this.timeAdded = timeAdded;
  }

  /**
   * @param vendorCode the vendorCode to set
   */
  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("DistributionTaskFilterDTO [productName=");
    builder.append(productName);
    builder.append(", categoryCode=");
    builder.append(categoryCode);
    builder.append(", businessPartnerCode=");
    builder.append(businessPartnerCode);
    builder.append(", rejected=");
    builder.append(rejected);
    builder.append(", vendorCode=");
    builder.append(vendorCode);
    builder.append(", status=");
    builder.append(status);
    builder.append(", timeAdded=");
    builder.append(timeAdded);
    builder.append(", storeId=");
    builder.append(storeId);
    builder.append("]");
    return builder.toString();
  };

}
