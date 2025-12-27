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
@Table(name = ProductImagePrediction.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {GdnBaseEntity.STORE_ID, ProductImagePrediction.COLUMN_PREDICTION_TYPE})})
public class ProductImagePrediction extends GdnBaseEntity {

  private static final long serialVersionUID = -2096509291516895529L;
  public static final String TABLE_NAME = "PRD_PRODUCT_IMAGE_PREDICTION";
  public static final String COLUMN_PREDICTION_TYPE = "PREDICTION_TYPE";
  public static final String COLUMN_DISPLAY_NAME = "DISPLAY_NAME";
  public static final String COLUMN_DISPLAY_NAME_IN = "DISPLAY_NAME_IN";
  public static final String COLUMN_CONFIDENCE_THRESHOLD = "CONFIDENCE_THRESHOLD";
  public static final String COLUMN_PREDICTION_WEIGHTAGE = "PREDICTION_WEIGHTAGE";
  public static final String COLUMN_FORCE_REVIEW = "FORCE_REVIEW";
  public static final String COLUMN_NEED_REVISION_THRESHOLD = "NEED_REVISION_CONFIDENCE_THRESHOLD";
  public static final String COLUMN_NEED_REVISION_ENABLED = "NEED_REVISION_ENABLED";
  public static final String COLUMN_TYPE = "TYPE";
  public static final String COLUMN_TEXT_CONFIDENCE_THRESHOLD = "TEXT_CONFIDENCE_THRESHOLD";
  public static final String COLUMN_COMPARE_CATEGORY = "COMPARE_CATEGORY";
  public static final String PREDICTION_CONSIDERED = "PREDICTION_CONSIDERED";
  public static final String RElAX_TRUSTED_SELLER = "RELAX_TRUSTED_SELLER";

  @Column(name = COLUMN_PREDICTION_TYPE, nullable = false)
  private String predictionType;

  @Column(name = COLUMN_DISPLAY_NAME, nullable = false)
  private String displayName;

  @Column(name = COLUMN_DISPLAY_NAME_IN, nullable = false)
  private String displayNameIn;

  @Column(name = COLUMN_CONFIDENCE_THRESHOLD, nullable = false)
  private int confidenceThreshold;

  @Column(name = COLUMN_PREDICTION_WEIGHTAGE, nullable = false)
  private int predictionWeightage;

  @Column(name = COLUMN_FORCE_REVIEW)
  private boolean forceReview;

  @Column(name = COLUMN_NEED_REVISION_THRESHOLD, nullable = false)
  private int needRevisionConfidenceThreshold;

  @Column(name = COLUMN_NEED_REVISION_ENABLED)
  private boolean needRevisionEnabled;

  @Column(name = COLUMN_TYPE)
  private String type;

  @Column(name = COLUMN_TEXT_CONFIDENCE_THRESHOLD)
  private int textConfidenceThreshold;

  @Column(name = COLUMN_COMPARE_CATEGORY)
  private boolean compareCategory;

  @Column(name = PREDICTION_CONSIDERED)
  private boolean predictionConsidered;

  @Column(name = RElAX_TRUSTED_SELLER)
  private boolean relaxTrustedSeller;

}