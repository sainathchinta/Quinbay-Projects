package com.gdn.mta.bulk.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.springframework.data.annotation.LastModifiedDate;


import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import java.util.Date;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
@Entity
@EqualsAndHashCode(callSuper=true)
@Table(name = BulkProcessDataEstimation.TABLE_NAME)
public class BulkProcessDataEstimation extends GdnBaseEntity{

  public static final String TABLE_NAME = "BLP_BULK_PROCESS_DATA_ESTIMATION";
  public static final String COLUMN_PROCESS_TYPE = "PROCESS_TYPE";
  public static final String COLUMN_PROCESS_LEVEL_FETCH = "PROCESS_LEVEL_FETCH";
  public static final String COLUMN_DELTA_TIME_ESTIMATIONS = "DELTA_TIME_ESTIMATIONS";
  public static final String COLUMN_LAST_FETCH_TIME = "LAST_FETCH_TIME";

  @Column(name = COLUMN_PROCESS_TYPE ,nullable = false)
  private String processType;

  @Column(name = COLUMN_PROCESS_LEVEL_FETCH)
  private boolean processLevelFetch;

  @Column(name = COLUMN_DELTA_TIME_ESTIMATIONS)
  private String deltaTimeEstimations;

  @Column(name = COLUMN_LAST_FETCH_TIME)
  @LastModifiedDate
  private Date lastFetchTime;


}
