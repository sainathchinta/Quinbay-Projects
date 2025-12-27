package com.gdn.x.product.model.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.EnumType;
import javax.persistence.Enumerated;

import com.gdn.x.product.enums.DistributionStatus;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Document(collection = Product.DOCUMENT_NAME)
public class Product extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 1L;

  public static final String DOCUMENT_NAME = "prd_product";

  @Indexed(unique = true)
  @Field(value = ProductFieldNames.PRODUCT_SKU)
  private String productSku;

  @Field(value = ProductFieldNames.PRODUCT_CODE)
  private String productCode;

  @Enumerated(EnumType.STRING)
  @Field(value = ProductFieldNames.DISTRIBUTION_STATUS)
  private DistributionStatus distributionStatus = DistributionStatus.NON_DISTRIBUTION;

  @Enumerated(EnumType.STRING)
  @Field(value = ProductFieldNames.PRODUCT_TYPE)
  private ProductType productType;

  @Enumerated(EnumType.STRING)
  @Field(value = ProductFieldNames.CURATION_STATUS)
  private CurationStatus curationStatus;

  @Transient
  private String settlementType;

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

  @Transient
  private List<ProductAttributeDetail> descriptiveAttributes =
      new ArrayList<>();

  @Field(value = ProductFieldNames.PRODUCT_SPECIAL_ATTRIBUTES)
  private List<ProductSpecialAttribute> productSpecialAttributes;

  @Field(value = ProductFieldNames.MASTER_CATALOG)
  private MasterCatalog masterCatalog;

  @Field(value = ProductFieldNames.SALES_CATALOGS)
  private List<SalesCatalog> salesCatalogs = new ArrayList<>();

  @Field(value = ProductFieldNames.MASTER_DATA_PRODUCT)
  private MasterDataProduct masterDataProduct;

  @Field(value = ProductFieldNames.SALES_CATEGORY_SEQUENCES)
  private List<SalesCategorySequence> salesCategorySequences;

  @Transient
  private List<ItemCatalogVO> itemCatalogs;

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

  @Field(value = ProductFieldNames.FBB_ACTIVATED)
  private boolean fbbActivated;

  @Field(value = ProductFieldNames.B2C_ACTIVATED)
  private Boolean b2cActivated;

  @Field(value = ProductFieldNames.B2B_ACTIVATED)
  private boolean b2bActivated;

  @Field(value = ProductFieldNames.BUNDLE_PRODUCT)
  private boolean bundleProduct;

  @Field(value = ProductFieldNames.PICKED_FOR_DELETION)
  private boolean pickedForDeletion;

  @Field(value = ProductFieldNames.SIZE_CHART_CODE)
  private String sizeChartCode;

  @Field(value = ProductFieldNames.DIMENSIONS_MISSING)
  private Boolean dimensionsMissing;

  @Transient
  private String sizeAttributeCode;

  @Field(value = ProductFieldNames.MISSING_FIELDS)
  private Set<String> missingFields = new HashSet<>();

  @Transient
  private boolean sizeChartChanged;

  @Transient
  private boolean sharedProduct;

  @Transient
  private boolean imeiRequired;

  @Transient
  private Set<String> updatedFields = new HashSet<>();

  @Field(value = ProductFieldNames.VIDEO)
  private Video video;

  @Field(value = ProductFieldNames.YOUTUBE_URL)
  private String url;

  public Product(String productSku) {
    this.productSku = productSku;
  }

  public Product(String productSku, String productCode) {
    this.productSku = productSku;
    this.productCode = productCode;
  }

  public Product(String productSku, String productCode, ProductType productType,
      String settlementType, String merchantCode, boolean isSynchronized, String productCatentryId,
      List<ProductAttribute> definingAttributes,
      List<ProductSpecialAttribute> productSpecialAttributes, MasterCatalog masterCatalog,
      List<SalesCatalog> salesCatalogs, MasterDataProduct masterDataProduct) {
    this.productSku = productSku;
    this.productCode = productCode;
    this.productType = productType;
    this.settlementType = settlementType;
    this.merchantCode = merchantCode;
    this.isSynchronized = isSynchronized;
    this.productCatentryId = productCatentryId;
    this.definingAttributes = definingAttributes;
    this.productSpecialAttributes = productSpecialAttributes;
    this.masterCatalog = masterCatalog;
    this.salesCatalogs = salesCatalogs;
    this.masterDataProduct = masterDataProduct;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public List<SalesCatalog> getSalesCatalogs() {
    return Optional.ofNullable(salesCatalogs).orElse(new ArrayList<>()).stream()
        .filter(salesCatalog -> Constants.SALES_CATEGORY_CATALOG_CODE.equalsIgnoreCase(salesCatalog.getCatalogCode()))
        .collect(Collectors.toList());
  }

  public List<SalesCatalog> getSalesCatalogByCatalogCode(String catalogCode) {
    return Optional.ofNullable(salesCatalogs).orElse(new ArrayList<>()).stream()
        .filter(salesCatalog -> catalogCode.equalsIgnoreCase(salesCatalog.getCatalogCode()))
        .collect(Collectors.toList());
  }

  public List<SalesCatalog> getAllSalesCatalogs() {
    return this.salesCatalogs;
  }

  public Boolean getB2cActivated() {
    return Boolean.FALSE.equals(b2cActivated) ? b2cActivated : true;
  }
}
