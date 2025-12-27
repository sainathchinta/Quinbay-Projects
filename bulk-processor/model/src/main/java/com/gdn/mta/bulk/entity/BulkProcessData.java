package com.gdn.mta.bulk.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper=true)
@Entity
@Table(name = BulkProcessData.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    BulkProcessData.COLUMN_BULK_PROCESS_CODE, BulkProcessData.COLUMN_ROW_NUMBER})})
public class BulkProcessData extends GdnBaseEntity {

  private static final long serialVersionUID = -3721712155271881814L;
  public static final String TABLE_NAME = "BLP_BULK_PROCESS_DATA";
  public static final String COLUMN_BULK_PROCESS_ID = "BULK_PROCESS_ID";
  public static final String COLUMN_BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
  public static final String COLUMN_BULK_REQUEST_DATA = "BULK_REQUEST_DATA";
  public static final String COLUMN_ROW_NUMBER = "ROW_NUMBER";
  public static final String COLUMN_START_DATE = "START_DATE";
  public static final String COLUMN_END_DATE = "END_DATE";
  public static final String COLUMN_PARENT_PRODUCT = "PARENT_PRODUCT";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_REQUEST_ID = "REQUEST_ID";
  public static final String COLUMN_INPUT_ERROR = "INPUT_ERROR_COUNT";
  public static final String COLUMN_SYSTEM_ERROR = "SYSTEM_ERROR_COUNT";
  public static final String COLUMN_ERROR_MESSAGE = "ERROR_MESSAGE";
  public static final String COLUMN_NOTES = "NOTES";
  public static final String COLUMN_IDENTIFIER = "IDENTIFIER";

  public static final String STATUS_DATA_FETCH_PENDING = "DATA_FETCH_PENDING";
  public static final String STATUS_PENDING = "PENDING";
  public static final String STATUS_IN_PROGRESS = "IN_PROGRESS";
  public static final String STATUS_PROCESSING = "PROCESSING";
  public static final String STATUS_FINISHED = "FINISHED";
  public static final String STATUS_FAIL = "FAIL";
  public static final String STATUS_SUCCESS = "SUCCESS";
  public static final String STATUS_ABORTED = "ABORTED";
  public static final String STATUS_PARTIALLY_DONE = "PARTIALLY_COMPLETED";

  @Column(name = COLUMN_BULK_REQUEST_DATA, nullable = false)
  private String bulkRequestData;

  @Column(name = COLUMN_ROW_NUMBER)
  private Integer rowNumber;

  @Column(name = COLUMN_START_DATE)
  private Date startDate;

  @Column(name = COLUMN_END_DATE)
  private Date endDate;

  @Column(name = COLUMN_STATUS)
  private String status;

  @Column(name = COLUMN_PARENT_PRODUCT)
  private String parentProduct;

  @Column(name= COLUMN_REQUEST_ID)
  private String requestId;

  @Column(name = COLUMN_INPUT_ERROR)
  private Integer inputErrorCount;

  @Column(name = COLUMN_SYSTEM_ERROR)
  private Integer systemErrorCount;

  @Column(name = COLUMN_ERROR_MESSAGE)
  private String errorMessage;

  @Column(name = COLUMN_NOTES)
  private String notes;

  @Column(name = COLUMN_BULK_PROCESS_ID)
  private String bulkProcessId;

  @Column(name = COLUMN_BULK_PROCESS_CODE, nullable = false)
  private String bulkProcessCode;

  @Column(name = COLUMN_IDENTIFIER)
  private String identifier;
}
