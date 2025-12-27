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
@Table(name = ProductArchive.TABLE_NAME)
public class ProductArchive extends GdnBaseEntity {
  public static final String TABLE_NAME = "PCC_PRODUCT_ARCHIVE";
  public static final String COLUMN_PRODUCT_ID = "PRODUCT_ID";
  public static final String COLUMN_NAME = "NAME";
  public static final String COLUMN_PRODUCT_CODE = "PRODUCT_CODE";
  public static final String COLUMN_LENGTH = "LENGTH";
  public static final String COLUMN_WIDTH = "WIDTH";
  public static final String COLUMN_HEIGHT = "HEIGHT";
  public static final String COLUMN_WEIGHT = "WEIGHT";
  public static final String COLUMN_SHIPPING_WEIGHT = "SHIPPING_WEIGHT";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_LONG_DESCRIPTION = "LONG_DESCRIPTION";
  public static final String COLUMN_BRAND = "BRAND";
  public static final String COLUMN_UNIQUE_SELLING_POINT = "UNIQUE_SELLING_POINT";
  public static final String COLUMN_UOM = "UOM";
  public static final String COLUMN_ACTIVATED = "IS_ACTIVATED";
  public static final String COLUMN_PROMOSKU = "IS_PROMO_SKU";
  public static final String COLUMN_VIEWABLE = "IS_VIEWABLE";
  public static final String COLUMN_PRODUCT_STORY = "PRODUCT_STORY";
  public static final String COLUMN_SPECIFICATION_DETAIL = "SPECIFICATION_DETAIL";
  public static final String COLUMN_URL = "URL";
  public static final String COLUMN_FOR_REVIEW = "IS_FOR_REVIEW";
  public static final String COLUMN_REVIEW_PENDING = "REVIEW_PENDING";
  public static final String COLUMN_CREATED_MERCHANT = "CREATED_MERCHANT";
  public static final String COLUMN_EDITED = "EDITED";
  public static final String COLUMN_REVISED = "REVISED";

  @Column(name = COLUMN_PRODUCT_ID)
  private String productId;

  @Column(name = COLUMN_PRODUCT_CODE)
  private String productCode;

  @Column(name = COLUMN_NAME)
  private String name;

  @Column(name = COLUMN_LENGTH)
  private Double length;

  @Column(name = COLUMN_WIDTH)
  private Double width;

  @Column(name = COLUMN_HEIGHT)
  private Double height;

  @Column(name = COLUMN_WEIGHT)
  private Double weight;

  @Column(name = COLUMN_SHIPPING_WEIGHT)
  private Double shippingWeight;

  @Column(name = COLUMN_DESCRIPTION)
  private byte[] description;

  @Column(name = COLUMN_BRAND)
  private String brand;

  @Column(name = COLUMN_UNIQUE_SELLING_POINT)
  private String uniqueSellingPoint;

  @Column(name = COLUMN_UOM)
  private String uom;

  @Column(name = COLUMN_ACTIVATED)
  private boolean activated;

  @Column(name = COLUMN_VIEWABLE)
  private boolean viewable;

  @Column(name = COLUMN_PRODUCT_STORY)
  private String productStory;

  @Column(name = COLUMN_SPECIFICATION_DETAIL)
  private String specificationDetail;

  @Column(name = COLUMN_URL)
  private String url;

  @Column(name = COLUMN_FOR_REVIEW)
  private boolean forReview;

  @Column(name = COLUMN_PROMOSKU)
  private boolean promoSKU;

  @Column(name = COLUMN_REVIEW_PENDING)
  private boolean reviewPending;

  @Column(name = COLUMN_CREATED_MERCHANT)
  private String createdMerchant;

  @Column(name = COLUMN_EDITED)
  private boolean edited;

  @Column(name = COLUMN_REVISED)
  private boolean revised;
}
