package com.gdn.mta.bulk.entity;

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
@Table(name = BulkProcessImage.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {
    BulkProcessImage.COLUMN_BULK_PROCESS_CODE, BulkProcessImage.COLUMN_IMAGE_URL})})
public class BulkProcessImage extends GdnBaseEntity {

  private static final long serialVersionUID = -3721712155271881814L;
  public static final String TABLE_NAME = "BLP_BULK_PROCESS_IMAGE";
  public static final String COLUMN_BULK_PROCESS_ID = "BULK_PROCESS_ID";
  public static final String COLUMN_BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
  public static final String COLUMN_IMAGE_URL = "IMAGE_URL";
  public static final String COLUMN_LOCATION = "LOCATION";
  public static final String COLUMN_COMPLETED = "COMPLETED";
  public static final String COLUMN_ERROR_MESSAGE = "ERROR_MESSAGE";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";
  public static final String COLUMN_NOTES = "NOTES";

  @Column(name = COLUMN_IMAGE_URL, nullable = false)
  private String imageURL;

  @Column(name = COLUMN_LOCATION)
  private String location;

  @Column(name = COLUMN_COMPLETED)
  private boolean completed;

  @Column(name = COLUMN_ERROR_MESSAGE)
  private String errorMessage;

  @Column(name = COLUMN_BULK_PROCESS_ID)
  private String bulkProcessId;

  @Column(name = COLUMN_BULK_PROCESS_CODE, nullable = false)
  private String bulkProcessCode;

  @Column(name = COLUMN_SEQUENCE)
  private int sequence;

  @Column(name = COLUMN_NOTES)
  private String notes;
}
