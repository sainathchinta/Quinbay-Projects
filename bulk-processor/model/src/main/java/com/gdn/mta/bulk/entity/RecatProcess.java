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

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@EqualsAndHashCode(callSuper=true)
@Table(name = RecatProcess.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, RecatProcess.RECAT_REQUEST_CODE})})
public class RecatProcess extends GdnBaseEntity {
  public static final String TABLE_NAME = "blp_recat_process";

  public static final String RECAT_REQUEST_CODE = "RECAT_REQUEST_CODE";
  private static final String FILE_NAME = "FILE_NAME";
  private static final String SCHEDULED_TIME = "SCHEDULED_TIME";
  private static final String STATUS = "STATUS";
  private static final String TOTAL_COUNT = "TOTAL_COUNT";
  private static final String SUCCESS_COUNT = "SUCCESS_COUNT";
  private static final String ERROR_COUNT = "ERROR_COUNT";
  private static final String INPUT_ERROR_COUNT = "INPUT_ERROR_COUNT";
  private static final String SYSTEM_ERROR_COUNT = "SYSTEM_ERROR_COUNT";
  private static final String START_TIME = "START_TIME";
  private static final String END_TIME = "END_TIME";
  private static final String NOTES = "NOTES";

  private static final long serialVersionUID = -457598643151816563L;

  @Column(name = RecatProcess.RECAT_REQUEST_CODE, nullable = false)
  private String recatRequestCode;

  @Column(name = RecatProcess.FILE_NAME, nullable = false)
  private String fileName;

  @Column(name = RecatProcess.SCHEDULED_TIME, nullable = false)
  private Date scheduledTime;

  @Column(name = RecatProcess.STATUS, nullable = false)
  private String status;

  @Column(name = RecatProcess.TOTAL_COUNT)
  private Integer totalCount;

  @Column(name = RecatProcess.SUCCESS_COUNT)
  private Integer successCount;

  @Column(name = RecatProcess.ERROR_COUNT)
  private Integer errorCount;

  @Column(name = RecatProcess.INPUT_ERROR_COUNT)
  private Integer inputErrorCount;

  @Column(name = RecatProcess.SYSTEM_ERROR_COUNT)
  private Integer systemErrorCount;

  @Column(name = RecatProcess.START_TIME)
  private Date startTime;

  @Column(name = RecatProcess.END_TIME)
  private Date endTime;

  @OneToMany(cascade = CascadeType.ALL, mappedBy = "recatProcess", fetch = FetchType.LAZY)
  private List<ProductRecatStatus> productRecatStatus = new ArrayList<>();

  @Column(name = RecatProcess.NOTES)
  private String notes;

}