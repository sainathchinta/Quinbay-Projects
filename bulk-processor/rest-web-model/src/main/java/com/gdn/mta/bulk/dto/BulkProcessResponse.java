package com.gdn.mta.bulk.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessResponse extends BaseResponse {

  private static final long serialVersionUID = 2980459846714840678L;
  private String bulkProcessCode;
  private String bulkProcessType;
  private String businessPartnerCode;
  private Date startDate;
  private Date endDate;
  private String status;
  private String description;
  private List<BulkProcessNotesResponse> bulkProcessNotes = new ArrayList<BulkProcessNotesResponse>();

  public BulkProcessResponse() {
  }

  public BulkProcessResponse(String id, String storeId, Date createdDate, String createdBy, Date updatedDate,
      String updatedBy, String bulkProcessCode, String bulkProcessType, String businessPartnerCode, Date startDate,
      Date endDate, String status, String description, List<BulkProcessNotesResponse> bulkProcessNotes) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
    this.bulkProcessCode = bulkProcessCode;
    this.bulkProcessType = bulkProcessType;
    this.businessPartnerCode = businessPartnerCode;
    this.startDate = startDate;
    this.endDate = endDate;
    this.status = status;
    this.description = description;
    this.bulkProcessNotes = bulkProcessNotes;
  }

  public String getBulkProcessCode() {
    return bulkProcessCode;
  }

  public List<BulkProcessNotesResponse> getBulkProcessNotes() {
    return bulkProcessNotes;
  }

  public String getBulkProcessType() {
    return bulkProcessType;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public String getDescription() {
    return description;
  }

  public Date getEndDate() {
    return endDate;
  }

  public Date getStartDate() {
    return startDate;
  }

  public String getStatus() {
    return status;
  }

  public void setBulkProcessCode(String bulkProcessCode) {
    this.bulkProcessCode = bulkProcessCode;
  }

  public void setBulkProcessNotes(List<BulkProcessNotesResponse> bulkProcessNotes) {
    this.bulkProcessNotes = bulkProcessNotes;
  }

  public void setBulkProcessType(String bulkProcessType) {
    this.bulkProcessType = bulkProcessType;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public void setStartDate(Date startDate) {
    this.startDate = startDate;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  @Override
  public String toString() {
    return String
        .format(
            "BulkProcessResponse [bulkProcessCode=%s, bulkProcessType=%s, businessPartnerCode=%s, startDate=%s, endDate=%s, status=%s, description=%s, bulkProcessNotes=%s, getBulkProcessCode()=%s, getBulkProcessNotes()=%s, getBulkProcessType()=%s, getBusinessPartnerCode()=%s, getDescription()=%s, getEndDate()=%s, getStartDate()=%s, getStatus()=%s]",
            bulkProcessCode, bulkProcessType, businessPartnerCode, startDate, endDate, status, description,
            bulkProcessNotes, getBulkProcessCode(), getBulkProcessNotes(), getBulkProcessType(),
            getBusinessPartnerCode(), getDescription(), getEndDate(), getStartDate(), getStatus());
  }

}
