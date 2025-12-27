package com.gdn.x.productcategorybase.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = ProductImageCleanup.TABLE_NAME)
public class ProductImageCleanup extends GdnBaseEntity {

  private static final long serialVersionUID = 7121038348003315915L;
  public static final String TABLE_NAME = "PCC_PRODUCT_IMAGES_CLEANUP";
  public static final String COLUMN_LOCATION_PATH = "LOCATION_PATH";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";

  @Column(name = ProductImageCleanup.COLUMN_PRODUCT_CODE)
  private String productCode;

  @Column(name = ProductImageCleanup.COLUMN_LOCATION_PATH)
  private String locationPath;
}
