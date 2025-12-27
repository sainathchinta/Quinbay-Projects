package com.gdn.x.mta.distributiontask.model;

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = ProductReviewer.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, ProductReviewer.COLUMN_PRODUCT_CODE})})
public class ProductReviewer extends GdnBaseEntity {

  private static final long serialVersionUID = -2870780918004305965L;
  public static final String TABLE_NAME = "PDT_PRODUCT_REVIEWER";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_APPROVER_ASSIGNEE = "APPROVER_ASSIGNEE";
  public static final String COLUMN_ASSIGNED_DATE = "ASSIGNED_DATE";
  public static final String COLUMN_APPROVED_DATE = "APPROVED_DATE";

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_APPROVER_ASSIGNEE)
  private String approverAssignee;

  @Column(name = COLUMN_ASSIGNED_DATE)
  private Date assignedDate;

  @Column(name = COLUMN_APPROVED_DATE)
  private Date approvedDate;

}
