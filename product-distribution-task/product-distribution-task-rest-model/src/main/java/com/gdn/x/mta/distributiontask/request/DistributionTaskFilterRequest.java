package com.gdn.x.mta.distributiontask.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

/**
 * @author febryo.lesmana
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class DistributionTaskFilterRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;
  private String productName;
  private String categoryCode;
  private String businessPartnerCode;
  private Integer rejected;
  private String vendorCode;
  private String status;
  private Integer timeAdded;

  public DistributionTaskFilterRequest() {}

  public DistributionTaskFilterRequest(String productName, String categoryCode,
      String businessPartnerCode, Integer rejected, String vendorCode, String status,
      Integer timeAdded) {
    super();
    this.productName = productName;
    this.categoryCode = categoryCode;
    this.businessPartnerCode = businessPartnerCode;
    this.rejected = rejected;
    this.vendorCode = vendorCode;
    this.status = status;
    this.timeAdded = timeAdded;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public String getProductName() {
    return productName;
  }

  public Integer getRejected() {
    return rejected;
  }

  public String getStatus() {
    return status;
  }

  public Integer getTimeAdded() {
    return timeAdded;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setRejected(Integer rejected) {
    this.rejected = rejected;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  public void setTimeAdded(Integer timeAdded) {
    this.timeAdded = timeAdded;
  }

  public void setVendorCode(String vendorCode) {
    this.vendorCode = vendorCode;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("DistributionTaskFilterRequest [productName=");
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
    builder.append("]");
    return builder.toString();
  }


}
