package com.gdn.x.product.model.entity;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.InventoryType;
import com.gdn.x.product.enums.ProductFieldNames;

public class MasterDataItem implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.SKU_CODE)
  private String skuCode;

  @Field(value = ProductFieldNames.GENERATED_ITEM_NAME)
  private String generatedItemName;

  @Field(value = ProductFieldNames.UPC_CODE)
  private String upcCode;

  @Field(value = ProductFieldNames.IS_ACTIVATED)
  private boolean isActivated;

  @Field(value = ProductFieldNames.IS_VIEWABLE)
  private boolean isViewable;

  @Field(value = ProductFieldNames.HASH)
  private String hash;

  @Field(value = ProductFieldNames.INVENTORY_TYPE)
  private InventoryType inventoryType;

  @Field(value = ProductFieldNames.DANGEROUS_LEVEL)
  private Integer dangerousLevel;

  @Field(value = ProductFieldNames.MASTER_DATA_ITEM_ATTRIBUTE_VALUES)
  private List<MasterDataItemAttributeValue> masterDataItemAttributeValues = new ArrayList<>();

  @Field(value = ProductFieldNames.MASTER_DATA_ITEM_IMAGES)
  private List<MasterDataItemImage> masterDataItemImages = new ArrayList<>();

  @Field(value = ProductFieldNames.ITEM_DELIVERY_WEIGHT)
  private Double itemDeliveryWeight;

  @Field(value = ProductFieldNames.ITEM_HEIGHT)
  private Double itemHeight;

  @Field(value = ProductFieldNames.ITEM_LENGTH)
  private Double itemLength;

  @Field(value = ProductFieldNames.ITEM_WEIGHT)
  private Double itemWeight;

  @Field(value = ProductFieldNames.ITEM_WIDTH)
  private Double itemWidth;

  @Field(value = ProductFieldNames.PRODUCT_CODE)
  private String productCode;

  public MasterDataItem() {

  }

  public MasterDataItem(String skuCode, String generatedItemName, String upcCode,
      boolean isActivated, boolean isViewable, String hash,
      List<MasterDataItemAttributeValue> masterDataItemAttributeValues,
      List<MasterDataItemImage> masterDataItemImages, Double itemDeliveryWeight, Double itemHeight,
      Double itemLength, Double itemWeight, Double itemWidth, String productCode) {
    super();
    this.skuCode = skuCode;
    this.generatedItemName = generatedItemName;
    this.upcCode = upcCode;
    this.isActivated = isActivated;
    this.isViewable = isViewable;
    this.hash = hash;
    this.masterDataItemAttributeValues = masterDataItemAttributeValues;
    this.masterDataItemImages = masterDataItemImages;
    this.itemDeliveryWeight = itemDeliveryWeight;
    this.itemHeight = itemHeight;
    this.itemLength = itemLength;
    this.itemWeight = itemWeight;
    this.itemWidth = itemWidth;
    this.productCode = productCode;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public int getDangerousLevel() {
    return (this.dangerousLevel == null ? 0 : this.dangerousLevel);
  }

  public String getGeneratedItemName() {
    return this.generatedItemName;
  }

  public String getHash() {
    return this.hash;
  }

  public InventoryType getInventoryType() {
    return this.inventoryType;
  }

  public Double getItemDeliveryWeight() {
    return this.itemDeliveryWeight;
  }

  public Double getItemHeight() {
    return this.itemHeight;
  }

  public Double getItemLength() {
    return this.itemLength;
  }

  public Double getItemWeight() {
    return this.itemWeight;
  }

  public Double getItemWidth() {
    return this.itemWidth;
  }

  public List<MasterDataItemAttributeValue> getMasterDataItemAttributeValues() {
    return this.masterDataItemAttributeValues;
  }

  public List<MasterDataItemImage> getMasterDataItemImages() {
    return this.masterDataItemImages;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public String getSkuCode() {
    return this.skuCode;
  }

  public String getUpcCode() {
    return this.upcCode;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isActivated() {
    return this.isActivated;
  }

  private boolean isNotNullOrZero(Double value) {
    return value != null && value != 0.0;
  }

  public boolean isViewable() {
    return this.isViewable;
  }

  public void setActivated(boolean isActivated) {
    this.isActivated = isActivated;
  }

  public void setDangerousLevel(int dangerousLevel) {
    this.dangerousLevel = dangerousLevel;
  }

  public void setGeneratedItemName(String generatedItemName) {
    this.generatedItemName = generatedItemName;
  }

  public void setHash(String hash) {
    this.hash = hash;
  }

  public void setInventoryType(InventoryType inventoryType) {
    this.inventoryType = inventoryType;
  }

  public void setItemDeliveryWeight(Double itemDeliveryWeight) {
    this.itemDeliveryWeight = itemDeliveryWeight;
  }

  public void setItemDeliveryWeightIfNotNullOrZero(Double itemDeliveryWeight) {
    if (this.isNotNullOrZero(itemDeliveryWeight))
      this.itemDeliveryWeight = itemDeliveryWeight;
  }

  public void setItemHeight(Double itemHeight) {
    this.itemHeight = itemHeight;
  }

  public void setItemHeightIfNotNullOrZero(Double itemHeight) {
    if (this.isNotNullOrZero(itemHeight))
      this.itemHeight = itemHeight;
  }

  public void setItemLength(Double itemLength) {
    this.itemLength = itemLength;
  }

  public void setItemLengthIfNotNullOrZero(Double itemLength) {
    if (this.isNotNullOrZero(itemLength))
      this.itemLength = itemLength;
  }

  public void setItemWeight(Double itemWeight) {
    this.itemWeight = itemWeight;
  }

  public void setItemWeightIfNotNullOrZero(Double itemWeight) {
    if (this.isNotNullOrZero(itemWeight))
      this.itemWeight = itemWeight;
  }

  public void setItemWidth(Double itemWidth) {
    this.itemWidth = itemWidth;
  }

  public void setItemWidthIfNotNullOrZero(Double itemWidth) {
    if (this.isNotNullOrZero(itemWidth))
      this.itemWidth = itemWidth;
  }

  public void setMasterDataItemAttributeValues(
      List<MasterDataItemAttributeValue> itemAttributeValues) {
    this.masterDataItemAttributeValues = itemAttributeValues;
  }

  public void setMasterDataItemImages(List<MasterDataItemImage> masterDataItemImages) {
    this.masterDataItemImages = masterDataItemImages;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }

  public void setUpcCode(String upcCode) {
    this.upcCode = upcCode;
  }

  public void setViewable(boolean isViewable) {
    this.isViewable = isViewable;
  }

  @Override
  public String toString() {
    return String
        .format(
            "MasterDataItem [skuCode=%s, generatedItemName=%s, upcCode=%s, isActivated=%s, isViewable=%s, hash=%s, inventoryType=%s, dangerousLevel=%s, masterDataItemAttributeValues=%s, masterDataItemImages=%s, itemDeliveryWeight=%s, itemHeight=%s, itemLength=%s, itemWeight=%s, itemWidth=%s, productCode=%s, getClass()=%s, toString()=%s]",
            this.skuCode, this.generatedItemName, this.upcCode, this.isActivated, this.isViewable,
            this.hash, this.inventoryType, this.dangerousLevel, this.masterDataItemAttributeValues,
            this.masterDataItemImages, this.itemDeliveryWeight, this.itemHeight, this.itemLength,
            this.itemWeight, this.itemWidth, this.productCode, this.getClass(), super.toString());
  }
}
