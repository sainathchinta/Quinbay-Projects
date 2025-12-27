package com.gdn.partners.product.orchestrator.entity.product.level1;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.util.Date;

import com.gdn.GdnBaseEntity;

@Table(name = ProductLevel1.TABLE_NAME)
@Entity
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductLevel1 extends GdnBaseEntity {

  public static final String TABLE_NAME = "PRD_PRODUCT_COLLECTION";
  public static final String COLUMN_STORE_CODE ="BUSINESS_PARTNER_CODE";
  public static final String COLUMN_STORE_NAME = "BUSINESS_PARTNER_NAME";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_CODE = "PRODUCT_CODE";
  public static final String COLUMN_NAME = "PRODUCT_NAME";
  public static final String COLUMN_BRAND_CODE = "BRAND_CODE";
  public static final String COLUMN_BRAND_NAME = "BRAND";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_CATEGORY_NAME = "CATEGORY_NAME";
  public static final String COLUMN_ACTIVATED = "ACTIVATED";
  public static final String COLUMN_VIEWABLE = "VIEWABLE";
  public static final String COLUMN_STATE = "STATE";
  public static final String COLUMN_UPDATED_STATE_DATE = "UPDATED_STEP_DATE";
  public static final String COLUMN_SUBMITTED_DATE = "SUBMITTED_DATE";
  public static final String COLUMN_REVIEWER_NOTES = "REVIEWER_NOTES";
  public static final String FIELD_ID = "id";
  public static final String FIELD_STORE_ID = "storeId";
  public static final String FIELD_MARK_FOR_DELETE = "markForDelete";
  public static final String FIELD_CREATED_BY = "createdBy";
  public static final String FIELD_CREATED_DATE = "createdDate";
  public static final String FIELD_UPDATED_BY = "updatedBy";
  public static final String FIELD_UPDATED_DATE = "updatedDate";
  public static final String FIELD_STORE_CODE = "storeCode";
  public static final String FIELD_STORE_NAME = "storeName";
  public static final String FIELD_PRODUCT_ID = "productId";
  public static final String FIELD_CODE = "code";
  public static final String FIELD_NAME = "name";
  public static final String FIELD_BRAND_CODE = "brandCode";
  public static final String FIELD_BRAND_NAME = "brandName";
  public static final String FIELD_CATEGORY_CODE = "categoryCode";
  public static final String FIELD_CATEGORY_NAME = "categoryName";
  public static final String FIELD_ACTIVATED = "activated";
  public static final String FIELD_VIEWABLE = "viewable";
  public static final String FIELD_STATE = "state";
  public static final String FIELD_UPDATED_STATE_DATE = "updatedStateDate";
  public static final String FIELD_SUBMITTED_DATE = "submittedDate";
  public static final String FIELD_REVIEWER_NOTES = "reviewerNotes";
  public static final String FIELD_POST_LIVE = "POST_LIVE";

  @Column(name = ProductLevel1.COLUMN_STORE_CODE, nullable = false)
  private String storeCode;

  @Column(name = ProductLevel1.COLUMN_STORE_NAME, nullable = false)
  private String storeName;

  @Column(name = ProductLevel1.COLUMN_PRODUCT_ID, nullable = false)
  private String productId;

  @Column(name = ProductLevel1.COLUMN_CODE, nullable = false)
  private String code;

  @Column(name = ProductLevel1.COLUMN_NAME, nullable = false)
  private String name;

  @Column(name = ProductLevel1.COLUMN_BRAND_CODE, nullable = false)
  private String brandCode;

  @Column(name = ProductLevel1.COLUMN_BRAND_NAME, nullable = false)
  private String brandName;

  @Column(name = ProductLevel1.COLUMN_CATEGORY_CODE, nullable = false)
  private String categoryCode;

  @Column(name = ProductLevel1.COLUMN_CATEGORY_NAME, nullable = false)
  private String categoryName;

  @Column(name = ProductLevel1.COLUMN_ACTIVATED, nullable = false)
  @Builder.Default
  private boolean activated= false;

  @Column(name = ProductLevel1.COLUMN_VIEWABLE, nullable = false)
  @Builder.Default
  private boolean viewable = false;

  @Column(name = ProductLevel1.COLUMN_STATE, nullable = false)
  private String state;

  @Column(name = ProductLevel1.COLUMN_UPDATED_STATE_DATE, nullable = false)
  private Date updatedStateDate;

  @Column(name = ProductLevel1.COLUMN_SUBMITTED_DATE, nullable = false)
  private Date submittedDate;

  @Column(name = ProductLevel1.COLUMN_REVIEWER_NOTES)
  private String reviewerNotes;

  @Column(name = FIELD_POST_LIVE, nullable = false)
  private boolean postLive = false;
}
