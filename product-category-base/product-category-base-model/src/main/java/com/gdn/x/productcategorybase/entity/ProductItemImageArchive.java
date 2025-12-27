package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Entity
@Table(name = ProductItemImageArchive.TABLE_NAME)
public class ProductItemImageArchive extends GdnBaseEntity {
  public static final String TABLE_NAME = "PCC_PRODUCT_ITEM_IMAGES_ARCHIVE";
  public static final String COLUMN_LOCATION_PATH = "LOCATION_PATH";
  public static final String COLUMN_IS_MAIN_IMAGES = "IS_MAIN_IMAGE";
  public static final String COLUMN_IS_ORIGINAL_IMAGE = "ORIGINAL_IMAGE";
  public static final String PRODUCT_ITEM_ID = "PRODUCT_ITEM_ID";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";
  public static final String COLUMN_ACTIVE = "ACTIVE";
  public static final String COLUMN_HASH_CODE = "HASH_CODE";
  public static final String COLUMN_EDITED = "EDITED";
  public static final String COLUMN_REVISED = "REVISED";
  public static final String COMMON_IMAGE = "COMMON_IMAGE";

  @Column(name = PRODUCT_ITEM_ID)
  private String productItemId;

  @Column(name = COLUMN_IS_MAIN_IMAGES)
  private boolean isMainImages;

  @Column(name = COLUMN_LOCATION_PATH)
  private String locationPath;

  @Column(name = COLUMN_SEQUENCE)
  private Integer sequence;

  @Column(name = COLUMN_ACTIVE)
  private boolean active;

  @Column(name = COLUMN_HASH_CODE)
  private String hashCode;

  @Column(name = COLUMN_IS_ORIGINAL_IMAGE)
  private Boolean originalImage;

  @Column(name = COLUMN_EDITED)
  private boolean edited;

  @Column(name = COLUMN_REVISED)
  private boolean revised;

  @Column(name = COMMON_IMAGE)
  private boolean commonImage;
}
