package com.gdn.mta.bulk.models;

import java.io.Serializable;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AuditTrailInfo implements Serializable {
  private static final long serialVersionUID = -7286682881305750422L;
  private String requestId;
  private String username;
  private String remoteAddress;
  private String businessPartnerCode;
  private String fileName;
  private Set<String> accessiblePickupPointCodes;

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getRemoteAddress() {
    return remoteAddress;
  }

  public void setRemoteAddress(String remoteAddress) {
    this.remoteAddress = remoteAddress;
  }

  public String getFileName() {
    return fileName;
  }

  public void setFileName(String fileName) {
    this.fileName = fileName;
  }

  public Set<String> getAccessiblePickupPointCodes() {
    return accessiblePickupPointCodes;
  }

  public void setAccessiblePickupPointCodes(Set<String> accessiblePickupPointCodes) {
    this.accessiblePickupPointCodes = accessiblePickupPointCodes;
  }
}
