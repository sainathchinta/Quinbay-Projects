package com.gdn.x.product.model.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductType;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Document(collection = ProductArchive.DOCUMENT_NAME)
public class ProductArchive extends GdnBaseMongoEntity {
  public static final String DOCUMENT_NAME = "prd_product_archive";

  @Indexed(unique = true)
  @Field(value = ProductFieldNames.PRODUCT_SKU)
  private String productSku;

  @Field(value = ProductFieldNames.PRODUCT_CODE)
  private String productCode;

  @Enumerated(EnumType.STRING)
  @Field(value = ProductFieldNames.PRODUCT_TYPE)
  private ProductType productType;

  @Field(value = ProductFieldNames.MERCHANT_CODE)
  private String merchantCode;

  @Field(value = ProductFieldNames.IS_SYNCHRONIZED)
  private boolean isSynchronized;

  @Field(value = ProductFieldNames.IS_SUSPENDED)
  private boolean isSuspended;

  @Field(value = ProductFieldNames.PRODUCT_CATENTRY_ID)
  private String productCatentryId;

  @Field(value = ProductFieldNames.DEFINING_ATTRIBUTES)
  private List<ProductAttribute> definingAttributes = new ArrayList<>();

  @Field(value = ProductFieldNames.PRODUCT_SPECIAL_ATTRIBUTES)
  private List<ProductSpecialAttribute> productSpecialAttributes;

  @Field(value = ProductFieldNames.MASTER_CATALOG)
  private MasterCatalog masterCatalog;

  @Field(value = ProductFieldNames.SALES_CATALOGS)
  private List<SalesCatalog> salesCatalogs;

  @Field(value = ProductFieldNames.MASTER_DATA_PRODUCT)
  private MasterDataProduct masterDataProduct;

  @Field(value = ProductFieldNames.SALES_CATEGORY_SEQUENCES)
  private List<SalesCategorySequence> salesCategorySequences;

  @Field(value = ProductFieldNames.INSTALLATION_REQUIRED)
  private boolean installationRequired;

  @Field(value = ProductFieldNames.OFF2ON_ITEM_COUNT)
  private int off2OnItemCount;

  @Field(value = ProductFieldNames.TRADING_PRODUCT)
  private boolean tradingProduct;

  @Field(value = ProductFieldNames.FORCE_REVIEW)
  private boolean forceReview;

  @Field(value = ProductFieldNames.PRODUCT_SCORE)
  private ProductScore productScore;

  @Field(value = ProductFieldNames.PRODUCT_CENTER_UPDATED_DATE)
  private Date productCenterUpdatedDate;

  @Field(value = ProductFieldNames.IS_ARCHIVED)
  private boolean isArchived;

  @Field(value = ProductFieldNames.IS_ARCHIVED_BEFORE_SUSPENSION)
  private boolean isArchivedBeforeSuspension;

  @Field(value = ProductFieldNames.OFF2ON_CHANNEL_ACTIVE)
  private boolean off2OnChannelActive;

  @Field(value = ProductFieldNames.PREORDER)
  private PreOrder preOrder;

  @Field(value = ProductFieldNames.IS_TAKEN_DOWN)
  private boolean takenDown;

  @Field(value = ProductFieldNames.FREE_SAMPLE)
  private boolean freeSample;

  @Field(value = ProductFieldNames.PRODUCT_NAME)
  private String productName;

  @Field(value = ProductFieldNames.BRAND)
  private String brand;

  @Field(value = ProductFieldNames.CNC_ACTIVATED)
  private boolean cncActivated;

  @Field(value = ProductFieldNames.PICKUP_POINT_CODES)
  private Set<String> pickupPointCodes = new HashSet<>();

  @Field(value = ProductFieldNames.ONLINE)
  private boolean online;

  @Field(value = ProductFieldNames.CATEGORY_CODE)
  private String categoryCode;
}
