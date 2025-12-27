package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import com.gdn.GdnBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = ProductImageQcBacklog.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, ProductImageQcBacklog.COLUMN_PRODUCT_CODE})})
public class ProductImageQcBacklog extends GdnBaseEntity {

  private static final long serialVersionUID = -2096509291516895529L;
  public static final String TABLE_NAME = "PRD_PRODUCT_IMAGE_QC_BACKLOG";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_STATUS = "STATUS";
  public static final String COLUMN_IMAGE_VIOLATIONS = "IMAGE_VIOLATIONS";
  public static final String COLUMN_IMAGE_QC_RESPONSE = "IMAGE_QC_RESPONSE";

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_STATUS, nullable = false)
  private String status;

  @Column(name = COLUMN_IMAGE_VIOLATIONS)
  private String imageViolations;

  @Column(name = COLUMN_IMAGE_QC_RESPONSE)
  private String imageQcResponse;

}