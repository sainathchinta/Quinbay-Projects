package com.gdn.x.mta.distributiontask.rest.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;

import java.io.Serializable;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

/**
 * Created by virajjasani on 17/09/16.
 */

@Data
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductListRequest implements Serializable {
  private static final long serialVersionUID = -689520852754090053L;

  private String vendorCode;
  private String businessPartnerName;
  private String categoryCode;
  private String productCode;
  private String startDate;
  private String endDate;
  private String productName;
  private String businessPartnerCode;
  private String timeFilterType;
  private List<WorkflowWebState> workflowWebState;

  public ProductListRequest() {
    // no implementation
  }

  public ProductListRequest(String vendorCode, String businessPartnerName, String categoryCode,
      String productCode, String startDate, String endDate, String productName) {
    this.vendorCode = vendorCode;
    this.businessPartnerName = businessPartnerName;
    this.categoryCode = categoryCode;
    this.productCode = productCode;
    this.startDate = startDate;
    this.endDate = endDate;
    this.productName = productName;
  }

  public ProductListRequest(String vendorCode, String businessPartnerName, String categoryCode,
      String productCode, String startDate, String endDate, String productName,
      String businessPartnerCode) {
    this.vendorCode = vendorCode;
    this.businessPartnerName = businessPartnerName;
    this.categoryCode = categoryCode;
    this.productCode = productCode;
    this.startDate = startDate;
    this.endDate = endDate;
    this.productName = productName;
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getBusinessPartnerName() {
    return businessPartnerName;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public String getEndDate() {
    return endDate;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getStartDate() {
    return startDate;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setBusinessPartnerName(String businessPartnerName) {
    this.businessPartnerName = businessPartnerName;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setEndDate(String endDate) {
    this.endDate = endDate;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setStartDate(String startDate) {
    this.startDate = startDate;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public List<WorkflowWebState> getWorkflowWebState() {
    return workflowWebState;
  }

  public void setWorkflowWebState(
      List<WorkflowWebState> workflowWebState) {
    this.workflowWebState = workflowWebState;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ProductListRequest{");
    sb.append("vendorCode='").append(vendorCode).append('\'');
    sb.append(", businessPartnerName='").append(businessPartnerName).append('\'');
    sb.append(", categoryCode='").append(categoryCode).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", startDate='").append(startDate).append('\'');
    sb.append(", endDate='").append(endDate).append('\'');
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", timeFilterType='").append(timeFilterType).append('\'');
    sb.append(", workflowWebState=").append(workflowWebState);
    sb.append('}');
    return sb.toString();
  }
}
