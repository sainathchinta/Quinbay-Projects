package com.gdn.mta.product.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import com.gdn.GdnBaseEntity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = ProductItemWholesalePrice.TABLE_NAME)
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ProductItemWholesalePrice extends GdnBaseEntity {

  private static final long serialVersionUID = 1435785014995140151L;

  public static final String TABLE_NAME = "PRD_PRODUCT_ITEM_WHOLESALE_PRICE";
  public static final String COLUMN_PRODUCT_ITEM_ID = "PRODUCT_ITEM_ID";
  public static final String COLUMN_GDN_PRODUCT_ITEM_SKU = "ITEM_SKU";
  public static final String COLUMN_WHOLESALE_RULES = "WHOLESALE_RULES";
  public static final String COLUMN_ITEM_CODE = "ITEM_CODE";
  public static final String COLUMN_WHOLESALE_PRICE_ACTIVATED = "WHOLESALE_PRICE_ACTIVATED";
  public static final String COLUMN_UPDATE_PENDING = "UPDATE_PENDING";
  public static final String COLUMN_PICKUP_POINT_CODE = "PICKUP_POINT_CODE";

  @Column(name = COLUMN_PRODUCT_ITEM_ID, nullable = false)
  private String productItemId;

  @Column(name = COLUMN_GDN_PRODUCT_ITEM_SKU, nullable = false)
  private String itemSku;

  @Column(name = COLUMN_WHOLESALE_RULES)
  private String wholesaleRules;

  @Column(name = COLUMN_ITEM_CODE)
  private String itemCode;

  @Column(name = COLUMN_WHOLESALE_PRICE_ACTIVATED)
  private boolean wholesalePriceActivated;

  @Column(name = COLUMN_UPDATE_PENDING)
  private boolean updatePending;

  @Column(name = COLUMN_PICKUP_POINT_CODE)
  private String pickupPointCode;
}

