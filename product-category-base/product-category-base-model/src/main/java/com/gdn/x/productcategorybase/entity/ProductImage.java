package com.gdn.x.productcategorybase.entity;

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
@Table(name = ProductImage.TABLE_NAME)
public class ProductImage extends GdnBaseEntity {

  private static final long serialVersionUID = 7121038348003315915L;
  public static final String TABLE_NAME = "PCC_PRODUCT_IMAGES";
  public static final String COLUMN_LOCATION_PATH = "LOCATION_PATH";
  public static final String COLUMN_IS_MAIN_IMAGES = "IS_MAIN_IMAGE";
  public static final String COLUMN_IS_ORIGINAL_IMAGE = "ORIGINAL_IMAGE";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";
  public static final String COLUMN_ACTIVE = "ACTIVE";
  public static final String COLUMN_HASH_CODE = "HASH_CODE";
  public static final String COLUMN_EDITED = "EDITED";
  public static final String COLUMN_REVISED = "REVISED";
  public static final String COMMON_IMAGE = "COMMON_IMAGE";

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = ProductImage.COLUMN_PRODUCT_ID)
  private Product product;

  @Column(name = ProductImage.COLUMN_PRODUCT_ID, insertable = false, updatable = false)
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

  @Column(name = ProductImage.COLUMN_IS_ORIGINAL_IMAGE)
  private Boolean originalImage;

  @Column(name = ProductImage.COLUMN_EDITED)
  private boolean edited = false;

  @Column(name = ProductImage.COLUMN_REVISED, nullable = false)
  private boolean revised = false;

  @Column(name = ProductImage.COMMON_IMAGE, nullable = false)
  private boolean commonImage;

  public ProductImage(Product product, boolean isMainImages, String locationPath, Integer sequence, String storeId) {
    super();
    this.product = product;
    this.isMainImages = isMainImages;
    this.locationPath = locationPath;
    this.sequence = sequence;
    this.setStoreId(storeId);
  }

  public ProductImage(Product product, boolean isMainImages, String locationPath, Integer sequence, String storeId,
      boolean active, String hashCode) {
    this(product, isMainImages, locationPath, sequence, storeId);
    this.active = active;
    this.hashCode = hashCode;
  }

  @Override
  public String toString() {
    return String.format(
        "ProductImage [isMainImages=%s, locationPath=%s, sequence=%s, active=%s, originalImage=%s, hashCode=%s, commonImage=%s, id=%s, markForDelete=%s",isMainImages,
        locationPath, sequence, active, originalImage, hashCode, commonImage, getId(), isMarkForDelete());
  }
}
