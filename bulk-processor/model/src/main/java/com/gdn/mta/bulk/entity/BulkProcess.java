package com.gdn.mta.bulk.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import lombok.EqualsAndHashCode;

@Entity
@Table(name = BulkProcess.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {BulkProcess.COLUMN_BULK_PROCESS_CODE, BulkProcess.COLUMN_BULK_PROCESS_TYPE,
        BulkProcess.COLUMN_BUSINESS_PARTNER_CODE})})
@EqualsAndHashCode(callSuper=true)
public class
BulkProcess extends GdnBaseEntity {

  private static final long serialVersionUID = -3721712155271881814L;
  public static final String TABLE_NAME = "BLP_BULK_PROCESS";
  public static final String COLUMN_BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
  public static final String COLUMN_BULK_PROCESS_TYPE = "BULK_PROCESS_TYPE";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_START_DATE = "START_DATE";
  public static final String COLUMN_END_DATE = "END_DATE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_IS_BULK_UPDATE = "IS_BULK_UPDATE";
  public static final String COLUMN_SUCCESS_COUNT = "SUCCESS_COUNT";
  public static final String COLUMN_ERROR_COUNT = "ERROR_COUNT";
  public static final String COLUMN_TOTAL_COUNT = "TOTAL_COUNT";
  public static final String COLUMN_REQUEST_ID = "REQUEST_ID";
  public static final String COLUMN_INPUT_ERROR = "INPUT_ERROR_COUNT";
  public static final String COLUMN_SYSTEM_ERROR = "SYSTEM_ERROR_COUNT";
  public static final String COLUMN_IS_INTERNATIONAL_MERCHANT = "IS_INTERNATIONAL_MERCHANT";
  public static final String COLUMN_NOTES = "NOTES";
  public static final String COLUMN_UPLOADED_FILE = "UPLOADED_FILE";
  public static final String COLUMN_PRIMARY_IDENTIFIER = "PRIMARY_IDENTIFIER";

  public static final String STATUS_PENDING = "PENDING";
  public static final String STATUS_READY_TO_PROCESS = "READY_TO_PROCESS";
  public static final String STATUS_FAIL = "FAIL";
  public static final String STATUS_PROCESSED = "PROCESSED";
  public static final String STATUS_IN_PROGRESS = "IN_PROGRESS";
  public static final String STATUS_FINISHED = "FINISHED";
  public static final String STATUS_PICKED = "PICKED";
  public static final String STATUS_ABORTED = "ABORTED";
  public static final String STATUS_PARTIALLY_DONE = "PARTIALLY_COMPLETED";
  public static final String STATUS_IMAGE_PROCESSING = "IMAGE_PROCESSING";
  public static final String STATUS_IMAGE_AND_VIDEO_PROCESSING = "IMAGE_AND_VIDEO_PROCESSING";
  public static final String STATUS_IMAGE_PROCESSING_PRIORITY_1 = "IMAGE_PROCESSING_PRIORITY_1";
  public static final String STATUS_IMAGE_PROCESSING_PRIORITY_2 = "IMAGE_PROCESSING_PRIORITY_2";
  public static final String STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1 = "IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1";
  public static final String STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2 = "IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2";
  public static final String STATUS_PUBLISHED = "PUBLISHED";

  @Column(name = COLUMN_BULK_PROCESS_CODE, nullable = false)
  private String bulkProcessCode;

  @Column(name = COLUMN_BULK_PROCESS_TYPE, nullable = false)
  private String bulkProcessType;

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = COLUMN_START_DATE)
  private Date startDate;

  @Column(name = COLUMN_END_DATE)
  private Date endDate;

  @Column(name = COLUMN_STATUS)
  private String status;

  @Column(name = COLUMN_DESCRIPTION)
  private String description;

  @Column(name = COLUMN_IS_BULK_UPDATE)
  private Boolean isBulkUpdate;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "bulkProcess", fetch = FetchType.EAGER)
  private List<BulkProcessNotes> bulkProcessNotes = new ArrayList<>();

  @Column(name = COLUMN_SUCCESS_COUNT)
  private Integer successCount;

  @Column(name = COLUMN_ERROR_COUNT)
  private Integer errorCount;

  @Column(name = COLUMN_TOTAL_COUNT)
  private Integer totalCount;

  @Column(name = COLUMN_REQUEST_ID)
  private String requestId;

  @Column(name = COLUMN_INPUT_ERROR)
  private Integer inputErrorCount;

  @Column(name = COLUMN_SYSTEM_ERROR)
  private Integer systemErrorCount;

  @Column(name = COLUMN_IS_INTERNATIONAL_MERCHANT)
  private Boolean isInternationalMerchant;

  @Column(name = COLUMN_NOTES)
  private String notes;

  @Column(name = COLUMN_UPLOADED_FILE)
  private String uploadedFile;

  @Column(name = COLUMN_PRIMARY_IDENTIFIER)
  private String primaryIdentifier;

  public Integer getSuccessCount() {
    return successCount;
  }

  public void setSuccessCount(Integer successCount) {
    this.successCount = successCount;
  }

  public Integer getErrorCount() {
    return errorCount;
  }

  public void setErrorCount(Integer errorCount) {
    this.errorCount = errorCount;
  }

  public Integer getTotalCount() {
    return totalCount;
  }

  public void setTotalCount(Integer totalCount) {
    this.totalCount = totalCount;
  }

  public BulkProcess() {
  }

  public BulkProcess(String bulkProcessCode, String bulkProcessType, String businessPartnerCode, Date startDate,
      Date endDate, String status, String description, List<BulkProcessNotes> bulkProcessNotes) {
    super();
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

  public List<BulkProcessNotes> getBulkProcessNotes() {
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

  public void setBulkProcessNotes(List<BulkProcessNotes> bulkProcessNotes) {
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

  public Boolean getBulkUpdate() {
    return isBulkUpdate;
  }

  public void setBulkUpdate(Boolean bulkUpdate) {
    isBulkUpdate = bulkUpdate;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public Integer getInputErrorCount() {
    return inputErrorCount;
  }

  public void setInputErrorCount(Integer inputErrorCount) {
    this.inputErrorCount = inputErrorCount;
  }

  public Integer getSystemErrorCount() {
    return systemErrorCount;
  }

  public void setSystemErrorCount(Integer systemErrorCount) {
    this.systemErrorCount = systemErrorCount;
  }

  public Boolean getInternationalMerchant() {
    return isInternationalMerchant;
  }

  public void setInternationalMerchant(Boolean internationalMerchant) {
    isInternationalMerchant = internationalMerchant;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public String getUploadedFile() {
    return uploadedFile;
  }

  public void setUploadedFile(String uploadedFile) {
    this.uploadedFile = uploadedFile;
  }

  public String getPrimaryIdentifier() {
    return primaryIdentifier;
  }

  public void setPrimaryIdentifier(String primaryIdentifier) {
    this.primaryIdentifier = primaryIdentifier;
  }


  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkProcess{");
    sb.append("bulkProcessCode='").append(bulkProcessCode).append('\'');
    sb.append(", bulkProcessType='").append(bulkProcessType).append('\'');
    sb.append(", businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", startDate=").append(startDate);
    sb.append(", endDate=").append(endDate);
    sb.append(", status='").append(status).append('\'');
    sb.append(", description='").append(description).append('\'');
    sb.append(", isBulkUpdate=").append(isBulkUpdate);
    sb.append(", successCount=").append(successCount);
    sb.append(", errorCount=").append(errorCount);
    sb.append(", totalCount=").append(totalCount);
    sb.append(", inputErrorCount=").append(inputErrorCount);
    sb.append(", systemErrorCount=").append(systemErrorCount);
    sb.append(", categoryCode=").append(notes);
    sb.append(", isInternationalMerchant=").append(isInternationalMerchant)
      .append(", uploadedFile=").append(uploadedFile);
    sb.append(", primaryIdentifier=").append(primaryIdentifier);
    sb.append('}');
    return sb.toString();
  }
}
