package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Entity
@Data
@EqualsAndHashCode(callSuper=true)
@NoArgsConstructor
@AllArgsConstructor
@Table(name = ProductImage.TABLE_NAME)
public class ProductImageSingle extends GdnBaseEntity {

  private static final long serialVersionUID = 7121038348003315915L;
  public static final String TABLE_NAME = "PCC_PRODUCT_IMAGES";
  public static final String COLUMN_LOCATION_PATH = "LOCATION_PATH";
  public static final String COLUMN_IS_MAIN_IMAGES = "IS_MAIN_IMAGE";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";
  public static final String COLUMN_ACTIVE = "ACTIVE";
  public static final String COLUMN_HASH_CODE = "HASH_CODE";
  public static final String COLUMN_EDITED = "EDITED";
  
  @Column(name = ProductImage.COLUMN_PRODUCT_ID)
  private String productId;

  @Column(name = ProductImage.COLUMN_IS_MAIN_IMAGES)
  private boolean isMainImages = false;

  @Column(name = ProductImage.COLUMN_LOCATION_PATH)
  private String locationPath;

  @Column(name = ProductImage.COLUMN_SEQUENCE)
  private Integer sequence;
  
  @Column(name = ProductImage.COLUMN_ACTIVE)
  private boolean active;
  
  @Column(name = ProductImage.COLUMN_HASH_CODE)
  private String hashCode;

  @Column(name = ProductItemImage.COLUMN_EDITED)
  private boolean edited = false;

  @Column(name = ProductImage.COLUMN_IS_ORIGINAL_IMAGE)
  private Boolean originalImage;
}