package com.gdn.mta.bulk.dto;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
public class BulkPendingProductResponse implements Serializable {
  private static final long serialVersionUID = -7781780697298453629L;
  private String id;
  private String businessPartnerCode;
  private String bulkProcessType;
  private String createdBy;
  private String bulkProcessCode;
  private String startDate;
  private String timeLapsed;
  private String isBulkUpdate;

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getBulkProcessType() {
    return bulkProcessType;
  }

  public void setBulkProcessType(String bulkProcessType) {
    this.bulkProcessType = bulkProcessType;
  }

  public String getCreatedBy() {
    return createdBy;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public String getBulkProcessCode() {
    return bulkProcessCode;
  }

  public void setBulkProcessCode(String bulkProcessCode) {
    this.bulkProcessCode = bulkProcessCode;
  }

  public String getStartDate() {
    return startDate;
  }

  public void setStartDate(String startDate) {
    this.startDate = startDate;
  }

  public String getTimeLapsed() {
    return timeLapsed;
  }

  public void setTimeLapsed(String timeLapsed) {
    this.timeLapsed = timeLapsed;
  }

  public String getIsBulkUpdate() {
    return isBulkUpdate;
  }

  public void setIsBulkUpdate(String isBulkUpdate) {
    this.isBulkUpdate = isBulkUpdate;
  }

  @Override
  public String toString() {
    return "BulkPendingProductResponse{" + "id='" + id + '\'' + ", businessPartnerCode='"
        + businessPartnerCode + '\'' + ", bulkProcessType='" + bulkProcessType + '\''
        + ", createdBy='" + createdBy + '\'' + ", bulkProcessCode='" + bulkProcessCode + '\''
        + ", startDate='" + startDate + '\'' + ", timeLapsed='" + timeLapsed + '\'' +
        ", isBulkUpdate='" + isBulkUpdate + '\'' + '}';
  }
}
