package com.gdn.mta.bulk.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessNotesResponse extends BaseResponse {

  private static final long serialVersionUID = 3451507867176861789L;
  private String bulkProcessCode;
  private String notes;
  private boolean isPromoNote;

  public BulkProcessNotesResponse() {
  }

  public BulkProcessNotesResponse(String id, String storeId, Date createdDate, String createdBy, Date updatedDate,
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

  public boolean isPromoNote() {
    return isPromoNote;
  }

  public void setPromoNote(boolean promoNote) {
    isPromoNote = promoNote;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkProcessNotesResponse{");
    sb.append("bulkProcessCode='").append(bulkProcessCode).append('\'');
    sb.append(", notes='").append(notes).append('\'');
    sb.append(", isPromoNote='").append(isPromoNote).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
