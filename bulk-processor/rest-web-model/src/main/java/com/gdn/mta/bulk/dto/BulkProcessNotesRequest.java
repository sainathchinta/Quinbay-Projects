package com.gdn.mta.bulk.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.util.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessNotesRequest extends BaseRequest {

  private static final long serialVersionUID = 3854949670408296219L;
  private String bulkProcessCode;
  private String notes;

  public BulkProcessNotesRequest() {
  }

  public BulkProcessNotesRequest(String id, String storeId, Date createdDate, String createdBy, Date updatedDate,
      String updatedBy, String bulkProcessCode, String notes) {
    super(id, storeId, createdDate, createdBy, updatedDate, updatedBy);
    this.bulkProcessCode = bulkProcessCode;
    this.notes = notes;
  }

  public String getBulkProcessCode() {
    return bulkProcessCode;
  }

  public String getNotes() {
    return notes;
  }

  public void setBulkProcessCode(String bulkProcessCode) {
    this.bulkProcessCode = bulkProcessCode;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  @Override
  public String toString() {
    return String.format(
        "BulkProcessNotesRequest [bulkProcessCode=%s, notes=%s, getBulkProcessCode()=%s, getNotes()=%s]",
        bulkProcessCode, notes, getBulkProcessCode(), getNotes());
  }

}
