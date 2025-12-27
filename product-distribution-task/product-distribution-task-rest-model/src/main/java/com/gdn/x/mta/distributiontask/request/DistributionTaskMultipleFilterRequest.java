package com.gdn.x.mta.distributiontask.request;

import com.gdn.common.web.base.BaseRequest;

import java.util.List;

/**
 * Created by apple on 14/11/16.
 */
public class DistributionTaskMultipleFilterRequest extends BaseRequest {

  private String productName;
  private String categoryCode;
  private String businessPartnerCode;
  private List<Integer> rejectedList;
  private List<String> vendorCodes;
  private List<String> statusList;
  private String startDate;
  private String endDate;
  private String timeFilterType;


  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setEndDate(String endDate) {
    this.endDate = endDate;
  }

  public String getEndDate() {
    return endDate;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }


  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }



  public void setRejectedList(List<Integer> rejectedList) {

    this.rejectedList = rejectedList;
  }

  public void setVendorCodes(List<String> vendorCodes) {
    this.vendorCodes = vendorCodes;
  }

  public void setStatusList(List<String> statusList) {
    this.statusList = statusList;
  }

  public List<Integer> getRejectedList() {
    return rejectedList;
  }

  public String getProductName() {
    return productName;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public String getStartDate() {
    return startDate;
  }

  public void setStartDate(String startDate) {
    this.startDate = startDate;
  }

  public List<String> getVendorCodes() {
    return vendorCodes;
  }

  public String getTimeFilterType() {
    return timeFilterType;
  }

  public void setTimeFilterType(String timeFilterType) {
    this.timeFilterType = timeFilterType;
  }

  public List<String> getStatusList() {
    return statusList;
  }

  public DistributionTaskMultipleFilterRequest() {
  }

  public DistributionTaskMultipleFilterRequest(String productName, String categoryCode,
      String businessPartnerCode, List<Integer> rejectedList, List<String> vendorCodes,
      List<String> statusList, String startDate, String endDate) {
    this.productName = productName;
    this.categoryCode = categoryCode;
    this.businessPartnerCode = businessPartnerCode;
    this.startDate = startDate;
    this.endDate = endDate;
    this.rejectedList = rejectedList;
    this.vendorCodes = vendorCodes;
    this.statusList = statusList;
  }

  @Override public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("DistributionTaskFilterRequest [productName=");
    builder.append(productName);
    builder.append(", categoryCode=");
    builder.append(categoryCode);
    builder.append(", businessPartnerCode=");
    builder.append(businessPartnerCode);
    builder.append(", rejected=");
    builder.append(rejectedList);
    builder.append(", vendorCode=");
    builder.append(vendorCodes);
    builder.append(", status=");
    builder.append(statusList);
    builder.append(", startDate=");
    builder.append(startDate);
    builder.append(", endDate=");
    builder.append(endDate);
    builder.append("]");
    return builder.toString();
  }


}
