package com.gdn.mta.bulk.dto;

import java.util.Date;
import java.util.Objects;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
public class BulkPendingProductsDTO {
  private String id;
  private String businessPartnerCode;
  private String bulkProcessType;
  private String createdBy;
  private String bulkProcessCode;
  private Date startDate;
  private Boolean isBulkUpdate;

  public String getBusinessPartnerCode() {
    if(Objects.isNull(businessPartnerCode)){
      return "NULL";
    }
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getBulkProcessType() {
    if(Objects.isNull(bulkProcessType)){
      return "NULL";
    }
    return bulkProcessType;
  }

  public void setBulkProcessType(String bulkProcessType) {
    this.bulkProcessType = bulkProcessType;
  }

  public String getStartDateString() {
    if(Objects.isNull(startDate)){
      return "NULL";
    }
    return startDate.toString();
  }

  public Date getStartDate() {
    return startDate;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  public String getId() {
    if(Objects.isNull(id)){
      return "NULL";
    }
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getCreatedBy() {
    if(Objects.isNull(createdBy)){
      return "NULL";
    }
    return createdBy;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public String getBulkProcessCode() {
    if(Objects.isNull(bulkProcessCode)){
      return "NULL";
    }
    return bulkProcessCode;
  }

  public void setBulkProcessCode(String bulkProcessCode) {
    this.bulkProcessCode = bulkProcessCode;
  }

  public Boolean getBulkUpdate() {
    return isBulkUpdate;
  }

  public String getBulkUpdateString() {
    if(Objects.isNull(isBulkUpdate)){
      return "NULL";
    }
    return isBulkUpdate.toString();
  }

  public void setBulkUpdate(Boolean bulkUpdate) {
    isBulkUpdate = bulkUpdate;
  }

  @Override
  public String toString() {
    return "BulkPendingProductsDTO{" + "id='" + id + '\'' + ", businessPartnerCode='"
        + businessPartnerCode + '\'' + ", bulkProcessType='" + bulkProcessType + '\''
        + ", createdBy='" + createdBy + '\'' + ", bulkProcessCode='" + bulkProcessCode + '\''
        + ", startDate=" + startDate + ", isBulkUpdate='" + isBulkUpdate + '\'' + '}';
  }
}
