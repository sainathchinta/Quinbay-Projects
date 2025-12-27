package com.gdn.x.product.model.entity;

import java.util.HashSet;
import java.util.Set;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Document(collection = ItemPickupPointArchive.DOCUMENT_NAME)
public class ItemPickupPointArchive extends GdnBaseMongoEntity {
  public static final String DOCUMENT_NAME = "prd_item_pickuppoint_archive";

  @Field(value = ProductFieldNames.OFFLINE_ITEM_ID)
  private String offlineItemId;

  @Field(value = ProductFieldNames.ITEM_SKU)
  private String itemSku;

  @Field(value = ProductFieldNames.MERCHANT_SKU)
  private String merchantSku;

  @Field(value = ProductFieldNames.PICKUP_POINT_CODE)
  private String pickupPointCode;

  @Field(value = ProductFieldNames.MERCHANT_CODE)
  private String merchantCode;

  @Field(value = ProductFieldNames.CNC_ACTIVE)
  private boolean cncActive;

  @Field(value = ProductFieldNames.EXTERNAL_PICKUP_POINT_CODE)
  private String externalPickupPointCode;

  @Field(value = ProductFieldNames.DELIVERY)
  private boolean delivery;

  @Field(value = ProductFieldNames.PRODUCT_SKU)
  private String productSku;

  @Field(value = ProductFieldNames.PRICE)
  private Set<Price> price = new HashSet<>();

  @Field(value = ProductFieldNames.ITEM_VIEW_CONFIGS)
  private Set<ItemViewConfig> itemViewConfig = new HashSet<>();

  @Field(value = ProductFieldNames.WHOLESALE_PRICE_EXISTS)
  private boolean wholesalePriceExists;

  @Field(value = ProductFieldNames.PROMO_BUNDLING)
  private boolean promoBundling;

  @Field(value = ProductFieldNames.ACTIVE_PROMO_BUNDLINGS)
  private Set<String> activePromoBundlings = new HashSet<>();

  @Field(value = ProductFieldNames.MERCHANT_PROMO_DISCOUNT)
  private boolean merchantPromoDiscount;

  @Field(value = ProductFieldNames.IS_FLASH_SALE_ACTIVE)
  private boolean flashSaleActive;
}
