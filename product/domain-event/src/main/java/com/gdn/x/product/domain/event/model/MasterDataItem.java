package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.domain.event.enums.InventoryType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataItem implements Serializable {

  private static final long serialVersionUID = 6525255931237187874L;

  private String skuCode;

  private String generatedItemName;

  private String upcCode;

  private boolean isActivated;

  private boolean isViewable;

  private String hash;

  private InventoryType inventoryType;

  private int dangerousLevel;

  private List<MasterDataItemAttributeValue> masterDataItemAttributeValues =
      new ArrayList<MasterDataItemAttributeValue>();

  private List<MasterDataItemImage> masterDataItemImages = new ArrayList<MasterDataItemImage>();

  private Double itemDeliveryWeight;

  private Double itemHeight;

  private Double itemLength;

  private Double itemWeight;

  private Double itemWidth;

  private String productCode;

  public MasterDataItem() {

  }

  public MasterDataItem(String skuCode, String generatedItemName, String upcCode,
      boolean isActivated, boolean isViewable, String hash,
      List<MasterDataItemAttributeValue> masterDataItemAttributeValues,
      List<MasterDataItemImage> masterDataItemImages, Double itemDeliveryWeight, Double itemHeight,
      Double itemLength, Double itemWeight, Double itemWidth) {
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
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public int getDangerousLevel() {
    return dangerousLevel;
  }

  public String getGeneratedItemName() {
    return this.generatedItemName;
  }

  public String getHash() {
    return this.hash;
  }

  public InventoryType getInventoryType() {
    return inventoryType;
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

  public void setItemHeight(Double itemHeight) {
    this.itemHeight = itemHeight;
  }

  public void setItemLength(Double itemLength) {
    this.itemLength = itemLength;
  }

  public void setItemWeight(Double itemWeight) {
    this.itemWeight = itemWeight;
  }

  public void setItemWidth(Double itemWidth) {
    this.itemWidth = itemWidth;
  }

  public void setMasterDataItemAttributeValues(
      List<MasterDataItemAttributeValue> itemAttributeValues) {
    this.masterDataItemAttributeValues = itemAttributeValues;
  }

  public void setMasterDataItemImages(List<MasterDataItemImage> masterDataItemImages) {
    this.masterDataItemImages = masterDataItemImages;
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

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("MasterDataItem{");
    sb.append("productCode='").append(productCode).append('\'');
    sb.append(", skuCode='").append(skuCode).append('\'');
    sb.append(", generatedItemName='").append(generatedItemName).append('\'');
    sb.append(", upcCode='").append(upcCode).append('\'');
    sb.append(", isActivated=").append(isActivated);
    sb.append(", isViewable=").append(isViewable);
    sb.append(", hash='").append(hash).append('\'');
    sb.append(", inventoryType=").append(inventoryType);
    sb.append(", dangerousLevel=").append(dangerousLevel);
    sb.append(", masterDataItemAttributeValues=").append(masterDataItemAttributeValues);
    sb.append(", masterDataItemImages=").append(masterDataItemImages);
    sb.append(", itemDeliveryWeight=").append(itemDeliveryWeight);
    sb.append(", itemHeight=").append(itemHeight);
    sb.append(", itemLength=").append(itemLength);
    sb.append(", itemWeight=").append(itemWeight);
    sb.append(", itemWidth=").append(itemWidth);
    sb.append('}');
    return sb.toString();
  }
}
