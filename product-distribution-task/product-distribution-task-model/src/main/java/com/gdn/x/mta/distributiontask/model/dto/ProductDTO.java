package com.gdn.x.mta.distributiontask.model.dto;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

import java.util.Date;
import java.util.List;

/**
 * Created by virajjasani on 15/09/16.
 */
public class ProductDTO {

  private String storeId;
  private String productCode;
  private String categoryName;
  private String categoryCode;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String vendorId;
  private Date startDate;
  private Date endDate;
  private Date startUpdatedDate;
  private Date endUpdatedDate;
  private List<WorkflowState> workflowState;
  private String productName;

  public ProductDTO() {
    // no implementation
  }

  public ProductDTO(Builder builder) {
    this.storeId = builder.storeId;
    this.productCode = builder.productCode;
    this.categoryName = builder.categoryName;
    this.categoryCode = builder.categoryCode;
    this.businessPartnerCode = builder.businessPartnerCode;
    this.businessPartnerName = builder.businessPartnerName;
    this.vendorId = builder.vendorId;
    this.startDate = builder.startDate;
    this.endDate = builder.endDate;
    this.workflowState = builder.workflowState;
    this.productName = builder.productName;
    this.startUpdatedDate = builder.startUpdatedDate;
    this.endUpdatedDate = builder.endUpdatedDate;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getBusinessPartnerName() {
    return businessPartnerName;
  }

  public void setBusinessPartnerName(String businessPartnerName) {
    this.businessPartnerName = businessPartnerName;
  }

  public String getVendorId() {
    return vendorId;
  }

  public void setVendorId(String vendorId) {
    this.vendorId = vendorId;
  }

  public Date getStartDate() {
    return startDate;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  public Date getEndDate() {
    return endDate;
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public Date getStartUpdatedDate() {
    return startUpdatedDate;
  }

  public void setStartUpdatedDate(Date startUpdatedDate) {
    this.startUpdatedDate = startUpdatedDate;
  }

  public Date getEndUpdatedDate() {
    return endUpdatedDate;
  }

  public void setEndUpdatedDate(Date endUpdatedDate) {
    this.endUpdatedDate = endUpdatedDate;
  }

  public List<WorkflowState> getWorkflowState() {
    return workflowState;
  }

  public void setWorkflowState(List<WorkflowState> workflowState) {
    this.workflowState = workflowState;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public static class Builder {
    private String storeId;
    private String productCode;
    private String categoryName;
    private String categoryCode;
    private String businessPartnerCode;
    private String businessPartnerName;
    private String vendorId;
    private Date startDate;
    private Date endDate;
    private Date startUpdatedDate;
    private Date endUpdatedDate;
    private List<WorkflowState> workflowState;
    private String productName;

    public Builder setWorkflowState(List<WorkflowState> workflowState) {
      this.workflowState = workflowState;
      return this;
    }

    public Builder setStoreId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public Builder setProductCode(String productCode) {
      this.productCode = productCode;
      return this;
    }

    public Builder setCategoryName(String categoryName) {
      this.categoryName = categoryName;
      return this;
    }

    public Builder setCategoryCode(String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public Builder setBusinessPartnerCode(String businessPartnerCode) {
      this.businessPartnerCode = businessPartnerCode;
      return this;
    }

    public Builder setBusinessPartnerName(String businessPartnerName) {
      this.businessPartnerName = businessPartnerName;
      return this;
    }

    public Builder setVendorId(String vendorId) {
      this.vendorId = vendorId;
      return this;
    }

    public Builder setStartDate(Date startDate) {
      this.startDate = startDate;
      return this;
    }

    public Builder setEndDate(Date endDate) {
      this.endDate = endDate;
      return this;
    }

    public Builder setStartUpdatedDate(Date startUpdatedDate) {
      this.startUpdatedDate = startUpdatedDate;
      return this;
    }

    public Builder setEndUpdatedDate(Date endUpdatedDate) {
      this.endUpdatedDate = endUpdatedDate;
      return this;
    }

    public Builder setProductName(String productName) {
      this.productName = productName;
      return this;
    }

    public ProductDTO build() {
      return new ProductDTO(this);
    }
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductDTO{");
    sb.append("storeId='").append(storeId).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", categoryName='").append(categoryName).append('\'');
    sb.append(", categoryCode='").append(categoryCode).append('\'');
    sb.append(", businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", businessPartnerName='").append(businessPartnerName).append('\'');
    sb.append(", vendorId='").append(vendorId).append('\'');
    sb.append(", startDate=").append(startDate);
    sb.append(", endDate=").append(endDate);
    sb.append(", startUpdatedDate=").append(startUpdatedDate);
    sb.append(", endUpdatedDate=").append(endUpdatedDate);
    sb.append(", workflowState=").append(workflowState);
    sb.append(", productName='").append(productName).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
