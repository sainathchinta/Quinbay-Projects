package com.gda.mta.product.dto;

import java.util.Date;

import org.apache.commons.lang3.builder.ToStringBuilder;


public class LogAuditTrailUpdatedProductRequest extends BaseRequest {

  private static final long serialVersionUID = 385029124554058381L;

  private Long pkLogAuditTrail;
  private Date accessTime;
  private String businessPartnerCode;
  private String changedBy;
  private String oldValues;
  private String newValues;
  private Boolean status;
  private String clientHost;
  private String gdnSku;
  private String activity;
  private String requestId;
  private String productSku;
  private String gdnName;

  public Long getPkLogAuditTrail() {
    return pkLogAuditTrail;
  }

  public void setPkLogAuditTrail(Long pkLogAuditTrail) {
    this.pkLogAuditTrail = pkLogAuditTrail;
  }

  public Date getAccessTime() {
    return accessTime;
  }

  public void setAccessTime(Date accessTime) {
    this.accessTime = accessTime;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getChangedBy() {
    return changedBy;
  }

  public void setChangedBy(String changedBy) {
    this.changedBy = changedBy;
  }

  public Boolean getStatus() {
    return status;
  }

  public void setStatus(Boolean status) {
    this.status = status;
  }

  public String getClientHost() {
    return clientHost;
  }

  public String getOldValues() {
    return oldValues;
  }

  public void setOldValues(String oldValues) {
    this.oldValues = oldValues;
  }

  public String getNewValues() {
    return newValues;
  }

  public void setNewValues(String newValues) {
    this.newValues = newValues;
  }

  public void setClientHost(String clientHost) {
    this.clientHost = clientHost;
  }

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public String getActivity() {
    return activity;
  }

  public void setActivity(String activity) {
    this.activity = activity;
  }
  
  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public String getProductSku() { return productSku; }

  public void setProductSku(String productSku) { this.productSku = productSku; }

  public String getGdnName() { return gdnName; }

  public void setGdnName(String gdnName) { this.gdnName = gdnName; }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }

}
