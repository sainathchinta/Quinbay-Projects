package com.gdn.x.mta.distributiontask.model.dto;

import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * Created by apple on 14/11/16.
 */
public class DistributionTaskMultipleFilterDTO implements Serializable {
  public static class Builder {
    private String productName;
    private String categoryCode;
    private String businessPartnerCode;
    private List<Integer> rejectedList;
    private List<String> vendorCodes;
    private List<WorkflowState> statusList;
    private Date startDate;
    private Date endDate;
    private String storeId;

    public DistributionTaskMultipleFilterDTO build() {
      return new DistributionTaskMultipleFilterDTO(this);
    }

    public DistributionTaskMultipleFilterDTO.Builder setBusinessPartnerCode(
        String businessPartnerCode) {
      this.businessPartnerCode = businessPartnerCode;
      return this;
    }

    public DistributionTaskMultipleFilterDTO.Builder setCategoryCode(String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public DistributionTaskMultipleFilterDTO.Builder setRejectedList(List<Integer> rejectedList) {
      this.rejectedList = rejectedList;
      return this;
    }

    public DistributionTaskMultipleFilterDTO.Builder setStatusList(List<WorkflowState> statusList) {
      this.statusList = statusList;
      return this;
    }

    public DistributionTaskMultipleFilterDTO.Builder setStoreId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public DistributionTaskMultipleFilterDTO.Builder setStartDate(Date startDate){
      this.startDate = startDate;
      return this;
    }

    public DistributionTaskMultipleFilterDTO.Builder setEndDate(Date endDate){
      this.endDate = endDate;
      return this;
    }

    public DistributionTaskMultipleFilterDTO.Builder setVendorCodes(List<String> vendorCodes) {
      this.vendorCodes = vendorCodes;
      return this;
    }

  }


  private static final long serialVersionUID = 1L;
  private String productName;
  private String categoryCode;
  private String businessPartnerCode;
  private List<Integer> rejectedList;
  private List<String> vendorCodes;
  private List<WorkflowState> statusList;
  private Date startDate;
  private Date endDate;
  private String storeId;
  private TimeFilterType timeFilterType;
  private String sortOrderByCreatedDate;

  public DistributionTaskMultipleFilterDTO() {
  }

  protected DistributionTaskMultipleFilterDTO(DistributionTaskMultipleFilterDTO.Builder builder) {
    this.productName = builder.productName;
    this.categoryCode = builder.categoryCode;
    this.businessPartnerCode = builder.businessPartnerCode;
    this.rejectedList = builder.rejectedList;
    this.vendorCodes = builder.vendorCodes;
    this.statusList = builder.statusList;
    this.startDate = builder.startDate;
    this.endDate = builder.endDate;
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
   * @return the rejectedList
   */
  public List<Integer> getRejectedList() {
    return rejectedList;
  }

  /**
   * @return the statusList
   */
  public List<WorkflowState> getStatusList() {
    return statusList;
  }

  public String getStoreId() {
    return storeId;
  }

  /**
   * @return the startDate
   */
  public Date getStartDate() {
    return startDate;
  }

  /**
   * @return the endDate
   */
  public Date getEndDate() {
    return endDate;
  }

  /**
   * @return the vendorCodes
   */
  public List<String> getVendorCodes() {
    return vendorCodes;
  }

  public TimeFilterType getTimeFilterType() {
    return timeFilterType;
  }

  public String getSortOrderByCreatedDate() {
    return sortOrderByCreatedDate;
  }

  /**
   * @param businessPartnerCode the businessPartnerCode to set
   */
  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  /**
   * @param startDate the startDate to set
   */
  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }


  /**
   * @param endDate the startDate to set
   */
  public void setEndDate(Date endDate) {
    this.endDate = endDate;
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
   * @param rejectedList the rejectedList to set
   */
  public void setRejectedList(List<Integer> rejectedList) {
    this.rejectedList = rejectedList;
  }

  /**
   * @param statusList the statusList to set
   */
  public void setStatusList(List<WorkflowState> statusList) {
    this.statusList = statusList;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  /**
   * @param vendorCodes the vendorCodes to set
   */
  public void setVendorCodes(List<String> vendorCodes) {
    this.vendorCodes = vendorCodes;
  }

  public void setTimeFilterType(TimeFilterType timeFilterType) {
    this.timeFilterType = timeFilterType;
  }

  public void setSortOrderByCreatedDate(String sortOrderByCreatedDate) {
    this.sortOrderByCreatedDate = sortOrderByCreatedDate;
  }

  @Override public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("DistributionTaskMultipleFilterDTO [productName=");
    builder.append(productName);
    builder.append(", categoryCode=");
    builder.append(categoryCode);
    builder.append(", businessPartnerCode=");
    builder.append(businessPartnerCode);
    builder.append(", rejectedList=");
    builder.append(rejectedList);
    builder.append(", vendorCodes=");
    builder.append(vendorCodes);
    builder.append(", statusList=");
    builder.append(statusList);
    builder.append(", startDate=");
    builder.append(startDate);
    builder.append(", endDate=");
    builder.append(endDate);
    builder.append(", storeId=");
    builder.append(storeId);
    builder.append("]");
    return builder.toString();
  }

  ;

}
