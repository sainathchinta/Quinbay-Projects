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
@Table(name = BulkNeedRevisionDeletionData.TABLE_NAME)
public class BulkNeedRevisionDeletionData extends GdnBaseEntity {
  private static final long serialVersionUID = -6702253367841603154L;
  public static final String TABLE_NAME = "BLP_NEED_REVISION_DELETION_DATA";
  public static final String COLUMN_NOTES = "NOTES";
  public static final String PRODUCT_CODE = "PRODUCT_CODE";
  public static final String ELIGIBLE_FOR_DELETION = "ELIGIBLE_FOR_DELETION";
  public static final String PRODUCT_DATA = "PRODUCT_DATA";

  @Column(name = BulkNeedRevisionDeletion.COLUMN_START_DATE)
  private Date startDate;

  @Column(name = BulkNeedRevisionDeletion.COLUMN_END_DATE)
  private Date endDate;

  @Column(name = BulkNeedRevisionDeletion.COLUMN_STATUS)
  private String status;

  @Column(name = BulkNeedRevisionDeletion.COLUMN_DELETION_PROCESS_CODE, nullable = false)
  private String deletionProcessCode;

  @Column(name = COLUMN_NOTES)
  private String notes;

  @Column(name = PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = ELIGIBLE_FOR_DELETION)
  private Boolean eligibleForDeletion;

  @Column(name = PRODUCT_DATA)
  private String productData;
}
