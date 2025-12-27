package com.gdn.mta.bulk.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = BulkNeedRevisionDeletion.TABLE_NAME)
public class BulkNeedRevisionDeletion extends GdnBaseEntity {
  private static final long serialVersionUID = -3721712155271881814L;
  public static final String TABLE_NAME = "BLP_NEED_REVISION_DELETION";
  public static final String COLUMN_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_PROCESS_TYPE = "PROCESS_TYPE";
  public static final String COLUMN_DELETION_PROCESS_CODE = "DELETION_PROCESS_CODE";
  public static final String COLUMN_START_DATE = "START_DATE";
  public static final String COLUMN_END_DATE = "END_DATE";
  public static final String COLUMN_ERROR_COUNT = "ERROR_COUNT";
  public static final String COLUMN_SUCCESS_COUNT = "SUCCESS_COUNT";
  public static final String COLUMN_TOTAL_COUNT = "TOTAL_COUNT";

  public static final String STATUS_PENDING = "PENDING";
  public static final String STATUS_READY_TO_PROCESS = "READY_TO_PROCESS";
  public static final String STATUS_FAIL = "FAIL";
  public static final String STATUS_PROCESSED = "PROCESSED";
  public static final String STATUS_IN_PROGRESS = "IN_PROGRESS";
  public static final String STATUS_FINISHED = "FINISHED";
  public static final String STATUS_PICKED = "PICKED";
  public static final String STATUS_ABORTED = "ABORTED";
  public static final String ABORTED_SELLER_NOT_ELIGIBLE = "ABORTED_SELLER_NOT_ELIGIBLE";
  public static final String STATUS_PARTIALLY_DONE = "PARTIALLY_COMPLETED";
  public static final String STATUS_PUBLISHED = "PUBLISHED";


  @Column(name = COLUMN_DELETION_PROCESS_CODE, nullable = false)
  private String deletionProcessCode;

  @Column(name = COLUMN_PROCESS_TYPE, nullable = false)
  private String processType;

  @Column(name = COLUMN_BUSINESS_PARTNER_CODE)
  private String businessPartnerCode;

  @Column(name = COLUMN_START_DATE)
  private Date startDate;

  @Column(name = COLUMN_END_DATE)
  private Date endDate;

  @Column(name = COLUMN_STATUS)
  private String status;


  @Column(name = COLUMN_SUCCESS_COUNT)
  private Integer successCount;

  @Column(name = COLUMN_ERROR_COUNT)
  private Integer errorCount;

  @Column(name = COLUMN_TOTAL_COUNT)
  private Integer totalCount;

}
