package com.gdn.mta.bulk.entity;

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
@Table(name = BulkInternalProcessData.TABLE_NAME)
public class BulkInternalProcessData extends GdnBaseEntity {
  public static final String TABLE_NAME = "BLP_INTERNAL_PROCESS_DATA";

  private static final String COLUMN_INTERNAL_PROCESS_REQUEST_CODE = "INTERNAL_PROCESS_REQUEST_CODE";
  public static final String COLUMN_PARENT_CODE = "PARENT_CODE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_ERROR_MESSAGE = "ERROR_MESSAGE";
  public static final String COLUMN_DATA = "DATA";
  public static final String COLUMN_SELLER_CODE = "SELLER_CODE";
  public static final String COLUMN_PROCESS_TYPE = "PROCESS_TYPE";
  private static final String COLUMN_INTERNAL_PROCESS_REQUEST_ID = "INTERNAL_PROCESS_REQUEST_ID";
  public static final String COLUMN_START_DATE = "START_DATE";
  public static final String COLUMN_END_DATE = "END_DATE";
  public static final String COLUMN_NOTES = "NOTES";

  @Column(name = COLUMN_INTERNAL_PROCESS_REQUEST_CODE, nullable = false)
  private String internalProcessRequestCode;

  @Column(name = COLUMN_PARENT_CODE)
  private String parentCode;

  @Column(name = COLUMN_STATUS)
  private String status;

  @Column(name = COLUMN_ERROR_MESSAGE)
  private String errorMessage;

  @Column(name = COLUMN_DATA)
  private String data;

  @Column(name = COLUMN_SELLER_CODE)
  private String sellerCode;

  @Column(name = COLUMN_INTERNAL_PROCESS_REQUEST_ID)
  private String internalProcessRequestId;

  @Column(name = COLUMN_PROCESS_TYPE)
  private String processType;

  @Column(name = COLUMN_NOTES)
  private String notes;
}
