package com.gdn.mta.bulk.entity;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@EqualsAndHashCode(callSuper=true)
@Table(name = BulkInternalProcess.TABLE_NAME)
public class BulkInternalProcess extends GdnBaseEntity {
  public static final String TABLE_NAME = "BLP_INTERNAL_PROCESS";

  private static final String COLUMN_INTERNAL_PROCESS_REQUEST_CODE = "INTERNAL_PROCESS_REQUEST_CODE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_SELLER_CODE = "SELLER_CODE";
  public static final String COLUMN_SELLER_NAME = "SELLER_NAME";
  public static final String COLUMN_FILE_NAME = "FILE_NAME";
  public static final String COLUMN_ERROR_FILE_PATH = "ERROR_FILE_PATH";
  public static final String COLUMN_SUCCESS_COUNT = "SUCCESS_COUNT";
  public static final String COLUMN_ERROR_COUNT = "ERROR_COUNT";
  public static final String COLUMN_TOTAL_COUNT = "TOTAL_COUNT";
  public static final String COLUMN_START_DATE = "START_TIME";
  public static final String COLUMN_PROCESS_TYPE = "PROCESS_TYPE";
  private static final String COLUMN_END_DATE = "END_TIME";
  private static final String COLUMN_NOTES = "NOTES";

  @Column(name = COLUMN_INTERNAL_PROCESS_REQUEST_CODE, nullable = false)
  private String internalProcessRequestCode;

  @Column(name = COLUMN_STATUS)
  private String status;

  @Column(name = COLUMN_SELLER_CODE)
  private String sellerCode;

  @Column(name = COLUMN_SELLER_NAME)
  private String sellerName;

  @Column(name = COLUMN_FILE_NAME)
  private String fileName;

  @Column(name = COLUMN_ERROR_FILE_PATH)
  private String errorFilePath;

  @Column(name = COLUMN_START_DATE)
  private Date startTime;

  @Column(name = COLUMN_TOTAL_COUNT)
  private Integer totalCount;

  @Column(name = COLUMN_SUCCESS_COUNT)
  private Integer successCount;

  @Column(name = COLUMN_ERROR_COUNT)
  private Integer errorCount;

  @Column(name = COLUMN_PROCESS_TYPE)
  private String processType;

  @Column(name = COLUMN_END_DATE)
  private Date endTime;

  @Column(name = COLUMN_NOTES)
  private String notes;
}
