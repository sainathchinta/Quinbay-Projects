package com.gdn.x.mta.distributiontask.model.dto;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

import java.util.Date;
import java.util.List;

/**
 * Created by virajjasani on 17/09/16.
 */
public class ProductListRequestDTO {
  private String vendorCode;
  private String categoryCode;
  private String productCode;
  private String businessPartnerName;
  private String businessPartnerCode;
  private List<WorkflowState> workflowState;
  private String productName;
  private String storeId;
  private Date startDate;
  private Date endDate;
  
  public ProductListRequestDTO(){}
  
  public ProductListRequestDTO(String vendorCode, String categoryCode, String productCode,
      String businessPartnerName, String storeId, Date startDate, Date endDate) {
    super();
    this.vendorCode = vendorCode;
    this.categoryCode = categoryCode;
    this.productCode = productCode;
    this.businessPartnerName = businessPartnerName;
    this.storeId = storeId;
    this.startDate = startDate;
    this.endDate = endDate;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getBusinessPartnerName() {
    return businessPartnerName;
  }

  public void setBusinessPartnerName(String businessPartnerName) {
    this.businessPartnerName = businessPartnerName;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
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

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
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

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductListRequestDTO{");
    sb.append("vendorCode='").append(vendorCode).append('\'');
    sb.append(", categoryCode='").append(categoryCode).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", businessPartnerName='").append(businessPartnerName).append('\'');
    sb.append(", businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", workflowState=").append(workflowState);
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", storeId='").append(storeId).append('\'');
    sb.append(", startDate=").append(startDate);
    sb.append(", endDate=").append(endDate);
    sb.append('}');
    return sb.toString();
  }

  @Override public boolean equals(Object o) {
    if (this == o)
      return true;
    if (!(o instanceof ProductListRequestDTO))
      return false;

    ProductListRequestDTO that = (ProductListRequestDTO) o;

    if (!getVendorCode().equals(that.getVendorCode()))
      return false;
    if (getCategoryCode() != null ?
        !getCategoryCode().equals(that.getCategoryCode()) :
        that.getCategoryCode() != null)
      return false;
    if (getProductCode() != null ?
        !getProductCode().equals(that.getProductCode()) :
        that.getProductCode() != null)
      return false;
    if (getBusinessPartnerName() != null ?
        !getBusinessPartnerName().equals(that.getBusinessPartnerName()) :
        that.getBusinessPartnerName() != null)
      return false;
    if (getBusinessPartnerCode() != null ?
        !getBusinessPartnerCode().equals(that.getBusinessPartnerCode()) :
        that.getBusinessPartnerCode() != null)
      return false;
    if (getWorkflowState() != that.getWorkflowState())
      return false;
    if (getProductName() != null ?
        !getProductName().equals(that.getProductName()) :
        that.getProductName() != null)
      return false;
    if (getStoreId() != null ? !getStoreId().equals(that.getStoreId()) : that.getStoreId() != null)
      return false;
    if (getWorkflowState() != that.getWorkflowState())
      return false;
    if (getProductName() != null ?
        !getProductName().equals(that.getProductName()) :
        that.getProductName() != null)
      return false;
    if (getStoreId() != null ? !getStoreId().equals(that.getStoreId()) : that.getStoreId() != null)
      return false;
    if (getStartDate() != null ?
        !getStartDate().equals(that.getStartDate()) :
        that.getStartDate() != null)
      return false;
    return getEndDate() != null ?
        getEndDate().equals(that.getEndDate()) :
        that.getEndDate() == null;

  }

  @Override public int hashCode() {
    int result = getVendorCode().hashCode();
    result = 31 * result + (getCategoryCode() != null ? getCategoryCode().hashCode() : 0);
    result = 31 * result + (getProductCode() != null ? getProductCode().hashCode() : 0);
    result =
        31 * result + (getBusinessPartnerName() != null ? getBusinessPartnerName().hashCode() : 0);
    result =
        31 * result + (getBusinessPartnerCode() != null ? getBusinessPartnerCode().hashCode() : 0);
    result = 31 * result + (getWorkflowState() != null ? getWorkflowState().hashCode() : 0);
    result = 31 * result + (getProductName() != null ? getProductName().hashCode() : 0);
    result = 31 * result + (getStoreId() != null ? getStoreId().hashCode() : 0);
    result = 31 * result + (getStartDate() != null ? getStartDate().hashCode() : 0);
    result = 31 * result + (getEndDate() != null ? getEndDate().hashCode() : 0);
    return result;
  }
}
