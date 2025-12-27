package com.gdn.x.product.model.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.vo.OfflineItemHistoryDetailVO;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Document(collection = ItemPickupPoint.DOCUMENT_NAME)
public class ItemPickupPoint extends GdnBaseMongoEntity {

  public static final String DOCUMENT_NAME = "prd_item_pickuppoint";

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

  @Field(value = ProductFieldNames.DISTRIBUTION)
  private boolean distribution;

  @Field(value = ProductFieldNames.EXTERNAL_PICKUP_POINT_CODE)
  private String externalPickupPointCode;

  @Field(value = ProductFieldNames.DELIVERY)
  private boolean delivery;

  @Field(value = ProductFieldNames.PRODUCT_SKU)
  private String productSku;

  @Field(value = ProductFieldNames.PRICE)
  private Set<Price> price = new HashSet<>();

  @Field(value = ProductFieldNames.PRICE_UPDATED_DATE)
  private Date priceUpdatedDate;

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

  @Field(value = ProductFieldNames.FBB_ACTIVATED)
  private boolean fbbActivated;

  @Field(value = ProductFieldNames.B2B_FIELDS)
  private B2bFields b2bFields;

  @Field(value = ProductFieldNames.FORCE_REVIEW)
  private boolean forceReview;

  @Field(value = ProductFieldNames.SUBSCRIBABLE)
  private boolean subscribable;

  @Field(value = ProductFieldNames.INSURED_AMOUNT)
  private double insuredAmount;

  @Transient
  private Boolean newData;

  @Transient
  private List<String> itemPickupPointDataChangeType = new ArrayList<>();

  @Transient
  private OfflineItemHistoryDetailVO offlineItemHistoryDetail;

  public Set<ItemViewConfig> getItemViewConfig() {
    return Optional.ofNullable(itemViewConfig).orElse(new HashSet<>()).stream()
        .filter(viewConfig -> Constants.DEFAULT.equalsIgnoreCase(viewConfig.getChannel()))
        .collect(Collectors.toSet());
  }

  public Set<ItemViewConfig> getItemViewConfigByChannel(String channel) {
    return Optional.ofNullable(itemViewConfig).orElse(new HashSet<>()).stream()
        .filter(viewConfig -> channel.equalsIgnoreCase(viewConfig.getChannel()))
        .collect(Collectors.toSet());
  }

  public ItemViewConfig getSingleItemViewConfigByChannel(String channel) {
    return Optional.ofNullable(itemViewConfig).orElse(new HashSet<>()).stream()
        .filter(viewConfig -> StringUtils.equals(channel, viewConfig.getChannel()))
        .findFirst().orElse(null);
  }

  public ItemViewConfig getSingleItemViewConfigByChannelDefaultEmpty(String channel) {
    return Optional.ofNullable(itemViewConfig).orElse(new HashSet<>()).stream()
        .filter(viewConfig -> StringUtils.equals(channel, viewConfig.getChannel()))
        .findFirst().orElse(new ItemViewConfig());
  }

  public Set<ItemViewConfig> getAllItemViewConfigs() {
    return itemViewConfig;
  }

  public void setItemViewConfig(Set<ItemViewConfig> itemViewConfig) {
    this.itemViewConfig = itemViewConfig;
  }

  @Override
  public String toString() {
    return "ItemPickupPoint{" + "offlineItemId='" + offlineItemId + '\'' + ", itemSku='" + itemSku + '\''
        + ", merchantSku='" + merchantSku + '\'' + ", pickupPointCode='" + pickupPointCode + '\'' + ", merchantCode='"
        + merchantCode + '\'' + ", cncActive=" + cncActive + ", externalPickupPointCode='" + externalPickupPointCode
        + '\'' + ", delivery=" + delivery + ", productSku='" + productSku + '\'' + ", price=" + price
        + ", itemViewConfig=" + itemViewConfig + ", wholesalePriceExists=" + wholesalePriceExists + ", promoBundling="
        + promoBundling + ", activePromoBundlings=" + activePromoBundlings + ", merchantPromoDiscount="
        + merchantPromoDiscount + ", flashSaleActive=" + flashSaleActive + ", fbbActivated=" + fbbActivated
        + ", forceReview=" + forceReview + ", newData=" + newData + ", offlineItemHistoryDetail="
        + offlineItemHistoryDetail + ", b2bFields=" + b2bFields + ", isSubscribable=" + subscribable + '}';
  }
}
