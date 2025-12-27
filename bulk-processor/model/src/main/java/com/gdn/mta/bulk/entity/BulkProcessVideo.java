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
@EqualsAndHashCode(callSuper = true)
@Entity
@Table(name = BulkProcessVideo.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {BulkProcessVideo.COLUMN_BULK_PROCESS_CODE, BulkProcessVideo.COLUMN_VIDEO_URL})})
public class BulkProcessVideo extends GdnBaseEntity {

  private static final long serialVersionUID = -3615593055354651939L;
  public static final String TABLE_NAME = "BLP_BULK_PROCESS_VIDEO";
  public static final String COLUMN_BULK_PROCESS_ID = "BULK_PROCESS_ID";
  public static final String COLUMN_BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
  public static final String COLUMN_UPLOADED_URL = "UPLOADED_URL";
  public static final String COLUMN_VIDEO_ID = "VIDEO_ID";
  public static final String COLUMN_VIDEO_URL = "VIDEO_URL";
  public static final String COLUMN_VIDEO_NAME = "VIDEO_NAME";
  public static final String COLUMN_COMPLETED = "COMPLETED";
  public static final String COLUMN_ERROR_MESSAGE = "ERROR_MESSAGE";
  public static final String COLUMN_ERROR_CODE = "ERROR_CODE";
  public static final String COLUMN_COVER_IMAGE_PATH = "COVER_IMAGE_PATH";

  @Column(name = COLUMN_UPLOADED_URL, nullable = false)
  private String uploadedURL;

  @Column(name = COLUMN_VIDEO_ID)
  private String videoId;

  @Column(name = COLUMN_VIDEO_URL)
  private String videoUrl;

  @Column(name = COLUMN_VIDEO_NAME)
  private String videoName;

  @Column(name = COLUMN_COMPLETED)
  private boolean completed;

  @Column(name = COLUMN_ERROR_MESSAGE)
  private String errorMessage;

  @Column(name = COLUMN_ERROR_CODE)
  private String errorCode;

  @Column(name = COLUMN_BULK_PROCESS_CODE, nullable = false)
  private String bulkProcessCode;

  @Column(name = COLUMN_COVER_IMAGE_PATH)
  private String coverImagePath;

}
