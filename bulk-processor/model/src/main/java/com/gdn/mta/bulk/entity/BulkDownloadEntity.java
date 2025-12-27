package com.gdn.mta.bulk.entity;

import org.apache.commons.lang3.builder.ToStringBuilder;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import java.util.Date;

/**
 * Created by virajjasani on 30/08/16.
 */
@Entity
@Table(name = BulkDownloadEntity.TABLE_NAME)
public class BulkDownloadEntity {

  public static final String TABLE_NAME = "BULK_DOWNLOAD_ENTITY";

  private static final String COLUMN_ID = "ID";
  private static final String COLUMN_REQUEST_ID = "REQUEST_ID";
  private static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String COLUMN_FILE_NAME = "FILE_NAME";
  private static final String COLUMN_CREATED_DATE = "CREATED_DATE";
  private static final String COLUMN_STATUS = "STATUS";
  private static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  private static final String COLUMN_REQUEST_BODY = "REQUEST_BODY";
  private static final String COLUMN_MARK_FOR_DELETE = "MARK_FOR_DELETE";
  private static final String COLUMN_CREATED_BY = "CREATED_BY";
  private static final String COLUMN_ENTITY_TYPE = "ENTITY_TYPE";
  private static final String COLUMN_RECORDS_DOWNLOAD = "RECORDS_DOWNLOAD";
  private static final String COLUMN_ERROR_MESSAGE = "ERROR_MESSAGE";
  private static final String COLUMN_END_DATE = "END_DATE";
  private static final String COLUMN_PRIMARY_IDENTIFIER = "PRIMARY_IDENTIFIER";



  public static final String ENTITY_TYPE_PRODUCTS = "PRODUCTS_DOWNLOAD";

  private Long id;
  private String requestId;
  private String businessPartnerCode;
  private String fileName;
  private Date createdDate;
  private String createdBy;
  private String status;
  private String description;
  private String requestBody;
  private String entityType;
  private boolean markForDelete;
  private Integer recordsDownload;
  private String errorMessage;
  private String primaryIdentifier;
  private Date endDate;

  @Id
  @SequenceGenerator(name = "BULK_DOWNLOAD_ENTITY_SEQ", allocationSize = 1, sequenceName =
      "BULK_DOWNLOAD_ENTITY_SEQ")
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BULK_DOWNLOAD_ENTITY_SEQ")
  @Column(name = COLUMN_ID)
  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  @Column(name = COLUMN_REQUEST_ID)
  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE)
  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  @Column(name = COLUMN_FILE_NAME)
  public String getFileName() {
    return fileName;
  }

  public void setFileName(String fileName) {
    this.fileName = fileName;
  }

  @Column(name = COLUMN_DESCRIPTION)
  public String getDescription() {
    return description;
  }

  @Column(name = COLUMN_REQUEST_BODY)
  public String getRequestBody() {
    return requestBody;
  }

  public void setRequestBody(String requestBody) {
    this.requestBody = requestBody;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Column(name = COLUMN_STATUS)
  public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }

  @Column(name = COLUMN_CREATED_BY)
  public String getCreatedBy() {
    return createdBy;
  }

  public void setCreatedBy(String createdBy) {
    this.createdBy = createdBy;
  }

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = COLUMN_CREATED_DATE)
  public Date getCreatedDate() {
    return createdDate;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  @Column(name = COLUMN_ENTITY_TYPE)
  public String getEntityType() {
    return entityType;
  }

  public void setEntityType(String entityType) {
    this.entityType = entityType;
  }

  @Column(name = COLUMN_MARK_FOR_DELETE)
  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  @Column(name = COLUMN_RECORDS_DOWNLOAD)
  public Integer getRecordsDownload() {
    return recordsDownload;
  }

  public void setRecordsDownload(Integer recordsDownload) {
    this.recordsDownload = recordsDownload;
  }

  @Column(name = COLUMN_ERROR_MESSAGE)
  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  @Temporal(TemporalType.TIMESTAMP)
  @Column(name = COLUMN_END_DATE)
  public Date getEndDate() {
    return endDate;
  }

  public void setEndDate(Date endDate) {
    this.endDate = endDate;
  }

  public void setPrimaryIdentifier(String primaryIdentifier) {
    this.primaryIdentifier = primaryIdentifier;
  }

  @Column(name = COLUMN_PRIMARY_IDENTIFIER)
  public String getPrimaryIdentifier() {
    return primaryIdentifier;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("id", id).append("requestId", requestId)
        .append("businessPartnerCode", businessPartnerCode).append("fileName", fileName)
        .append("createdDate", createdDate).append("createdBy", createdBy).append("status", status)
        .append("description", description).append("entityType", entityType).append("markForDelete", markForDelete)
        .append("recordsDownload", recordsDownload).append("errorMessage", errorMessage).append("endDate", endDate)
        .append("requestBody", requestBody).append("primaryIdentifier", primaryIdentifier).toString();
  }
}
