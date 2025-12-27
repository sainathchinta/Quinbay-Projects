package com.gdn.mta.bulk.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

/**
 * Created by virajjasani on 02/09/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDownloadProductResponse extends BaseResponse{

  private static final long serialVersionUID = -6054199496090900256L;

  private String requestId;
  private String businessPartnerCode;
  private String fileName;
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
        .append("status", status).append("description", description)
        .append("entityType", entityType).toString();
  }
}
