package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;

import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Entity
@Table(name = ProductAutoApproval.TABLE_NAME)
public class ProductAutoApproval extends GdnBaseEntity {

  private static final long serialVersionUID = 3971178911754687156L;
  public static final String TABLE_NAME = "PDT_PRODUCT_AUTO_APPROVAL";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_AUTO_APPROVAL_STATUS = "AUTO_APPROVAL_STATUS";
  public static final String COLUMN_RETRY_COUNT = "RETRY_COUNT";
  public static final String COLUMN_REASON_OF_FAILURE = "REASON_OF_FAILURE";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_AUTO_APPROVAL_STATUS, nullable = false)
  @Enumerated(EnumType.STRING)
  private AutoApprovalStatus autoApprovalStatus;

  @Column(name = COLUMN_RETRY_COUNT, nullable = false)
  private int retryCount;

  @Column(name = COLUMN_REASON_OF_FAILURE)
  private String reasonOfFailure;

  @Column(name = COLUMN_CATEGORY_CODE)
  private String categoryCode;

}
