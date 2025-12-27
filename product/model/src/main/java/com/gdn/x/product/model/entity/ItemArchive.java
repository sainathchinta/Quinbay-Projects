package com.gdn.x.product.model.entity;

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.DBRef;
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
@Document(collection = ItemArchive.DOCUMENT_NAME)
public class ItemArchive extends GdnBaseMongoEntity {

  public static final String DOCUMENT_NAME = "prd_item_archive";

  @Indexed(unique = true)
  @Field(value = ProductFieldNames.ITEM_SKU)
  private String itemSku;

  @Field(value = ProductFieldNames.PRODUCT_SKU)
  private String productSku;

  @Field(value = ProductFieldNames.MERCHANT_SKU)
  private String merchantSku;

  @Field(value = ProductFieldNames.MERCHANT_CODE)
  private String merchantCode;

  @Field(value = ProductFieldNames.ITEM_CODE)
  private String itemCode;

  @Field(value = ProductFieldNames.SOURCE_ITEM_CODE)
  private String sourceItemCode;

  @Field(value = ProductFieldNames.IS_SYNCHRONIZED)
  private boolean isSynchronized;

  @Field(value = ProductFieldNames.IS_ARCHIVED_BEFORE_SUSPENSION)
  private Boolean isArchivedBeforeSuspension;

  @Field(value = ProductFieldNames.ITEM_CATENTRY_ID)
  private String itemCatentryId;

  @Field(value = ProductFieldNames.MASTER_DATA_ITEM)
  private MasterDataItem masterDataItem;

  @DBRef(lazy = true)
  @Field(value = ProductFieldNames.PRISTINE_DATA_ITEM)
  private PristineDataItem pristineDataItem;

  @Field(value = ProductFieldNames.PRICE)
  private Set<Price> price = new HashSet<Price>();

  @Field(value = ProductFieldNames.ITEM_VIEW_CONFIGS)
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<ItemViewConfig>();

  @Field(value = ProductFieldNames.IS_LATE_FULFILLMENT)
  private Boolean isLateFulfillment = false;

  @Field(value = ProductFieldNames.PICKUP_POINT_CODE)
  private String pickupPointCode;

  @Field(value = ProductFieldNames.TICKET_TEMPLATE_CODE)
  private String ticketTemplateCode;

  @Field(value = ProductFieldNames.ETD_NOTE)
  private String etdNote;

  @Field(value = ProductFieldNames.OFF2ON_CHANNEL_ACTIVE)
  private boolean off2OnChannelActive;

  @Field(value = ProductFieldNames.IS_ARCHIVED)
  private boolean isArchived = false;

  @Field(value = ProductFieldNames.PROMO_BUNDLING)
  private boolean promoBundling = false;

  @Field(value = ProductFieldNames.CNC_ACTIVATED)
  private boolean cncActivated = false;

  @Field(value = ProductFieldNames.IS_FLASH_SALE_ACTIVE)
  private boolean isFlashSaleActive;

  @Field(value = ProductFieldNames.ACTIVE_PROMO_BUNDLINGS)
  private Set<String> activePromoBundlings;

  @Field(value = ProductFieldNames.LATE_FULFILLMENT_UPDATED_BY)
  private String lateFullfillmentUpdatedBy;

  @Field(value = ProductFieldNames.LATE_FULFILLMENT_UPDATED_DATE)
  private Date lateFullfillmentUpdatedDate;

  @Field(value = ProductFieldNames.FREE_SAMPLE)
  private boolean freeSample = false;

  @Field(value = ProductFieldNames.MERCHANT_PROMO_DISCOUNT)
  private boolean merchantPromoDiscount;

  @Field(value = ProductFieldNames.IS_SUBSCRIBABLE)
  private boolean isSubscribable;

  @Field(value = ProductFieldNames.FORCE_REVIEW)
  private boolean forceReview;

  @Field(value = ProductFieldNames.WHOLESALE_PRICE_EXISTS)
  private boolean wholesalePriceExists;

  @Field(value = ProductFieldNames.IS_CONTENT_CHANGED)
  private boolean isContentChanged;

  @Field(value = ProductFieldNames.INITIAL_CONTENT_CHANGED)
  private boolean initialContentChanged;

  @Field(value = ProductFieldNames.IS_ARCHIVED_BEFORE_EDIT)
  private boolean archivedBeforeEdit;

  @Field(value = ProductFieldNames.GENERATED_ITEM_NAME)
  private String generatedItemName;

  @Field(value = ProductFieldNames.CATEGORY_CODE)
  private String categoryCode;

  @Field(value = ProductFieldNames.MAIN_IMAGE_URL)
  private String mainImageUrl;

  @Field(value = ProductFieldNames.LENGTH)
  private double length;

  @Field(value = ProductFieldNames.WIDTH)
  private double width;

  @Field(value = ProductFieldNames.HEIGHT)
  private double height;

  @Field(value = ProductFieldNames.WEIGHT)
  private double weight;

  @Field(value = ProductFieldNames.SHIPPING_WEIGHT)
  private double shippingWeight;

  @Field(value = ProductFieldNames.DANGEROUS_LEVEL)
  private Integer dangerousLevel;

  @Field(value = ProductFieldNames.DEFINING_ATTRIBUTES)
  private List<ProductAttributeDetail> definingAttributes;

  @Field(value = ProductFieldNames.BRAND)
  private String brand;
}
