package com.gdn.mta.bulk.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@EqualsAndHashCode(callSuper=true)
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = ProductRecatStatus.TABLE_NAME)
public class ProductRecatStatus extends GdnBaseEntity {
  public static final String TABLE_NAME = "blp_product_recat_status";

  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String CATEGORY_NAME = "CATEGORY_NAME";
  private static final String NEW_CATEGORY_CODE = "NEW_CATEGORY_CODE";
  private static final String NEW_CATEGORY_NAME = "NEW_CATEGORY_NAME";
  private static final String RECAT_REQUEST_CODE = "RECAT_REQUEST_CODE";
  private static final String RECAT_REQUEST_ID = "RECAT_REQUEST_ID";
  private static final String STATUS = "STATUS";
  private static final String VALIDATION_ERROR = "VALIDATION_ERROR";
  private static final String SYSTEM_ERROR = "SYSTEM_ERROR";
  private static final String ERROR_MESSAGE = "ERROR_MESSAGE";

  private static final long serialVersionUID = 8315864245542495448L;

  @Column(name = ProductRecatStatus.PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = ProductRecatStatus.PRODUCT_NAME)
  private String productName;

  @Column(name = ProductRecatStatus.CATEGORY_CODE)
  private String categoryCode;

  @Column(name = ProductRecatStatus.CATEGORY_NAME)
  private String categoryName;

  @Column(name = ProductRecatStatus.NEW_CATEGORY_CODE)
  private String newCategoryCode;

  @Column(name = ProductRecatStatus.NEW_CATEGORY_NAME)
  private String newCategoryName;

  @Column(name = ProductRecatStatus.RECAT_REQUEST_CODE)
  private String recatRequestCode;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductRecatStatus.RECAT_REQUEST_ID)
  private RecatProcess recatProcess;

  @Column(name = ProductRecatStatus.STATUS)
  private String status;

  @Column(name = ProductRecatStatus.VALIDATION_ERROR)
  private boolean validationError;

  @Column(name = ProductRecatStatus.SYSTEM_ERROR)
  private boolean systemError;

  @Column(name = ProductRecatStatus.ERROR_MESSAGE)
  private String errorMessage;

}