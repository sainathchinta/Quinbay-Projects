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
@Table(name = ProductImageQcProcessingResponse.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, ProductImageQcProcessingResponse.COLUMN_PRODUCT_CODE})})
public class ProductImageQcProcessingResponse extends GdnBaseEntity {

  private static final long serialVersionUID = -2096509291516895529L;
  public static final String TABLE_NAME = "PRD_PRODUCT_IMAGE_PROCESSING_RESPONSE";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_PRODUCT_PREDICTION_SCORE = "PRODUCT_PREDICTION_SCORE";
  public static final String COLUMN_IMAGE_VIOLATIONS = "IMAGE_VIOLATIONS";
  public static final String COLUMN_TEXT_VIOLATIONS = "TEXT_VIOLATIONS";
  public static final String COLUMN_PREDICTED_BRAND = "PREDICTED_BRAND";
  public static final String COLUMN_IMAGE_QC_RESPONSE = "IMAGE_QC_RESPONSE";
  public static final String COLUMN_FORCE_REVIEW = "FORCE_REVIEW";

  @Column(name = COLUMN_PRODUCT_CODE, nullable = false)
  private String productCode;

  @Column(name = COLUMN_PRODUCT_PREDICTION_SCORE, nullable = false)
  private int productPredictionScore;

  @Column(name = COLUMN_IMAGE_VIOLATIONS)
  private String imageViolations;

  @Column(name = COLUMN_TEXT_VIOLATIONS)
  private String textViolations;

  @Column(name = COLUMN_PREDICTED_BRAND)
  private String predictedBrand;

  @Column(name = COLUMN_IMAGE_QC_RESPONSE)
  private String imageQcResponse;

  @Column(name = COLUMN_FORCE_REVIEW)
  private boolean forceReview;

}
