package com.gdn.mta.bulk.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Date;

/**
 * Created by virajjasani on 02/09/16.
 */
public class BulkDownloadProductDTO {

  private String requestId;
  private String businessPartnerCode;
  private String fileName;
  private Date createdDate;
  private String createdBy;
  private String status;
  private String description;
  private String entityType;

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getFileName() {
    return fileName;
  }

  public void setFileName(String fileName) {
    this.fileName = fileName;
  }

  public Date getCreatedDate() {
    return createdDate;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public String getCreatedBy() {
    return createdBy;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getEntityType() {
    return entityType;
  }

  public void setEntityType(String entityType) {
    this.entityType = entityType;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("requestId", requestId)
        .append("businessPartnerCode", businessPartnerCode).append("fileName", fileName)
        .append("createdDate", createdDate).append("createdBy", createdBy).append("status", status)
        .append("description", description).append("entityType", entityType).toString();
  }
}
