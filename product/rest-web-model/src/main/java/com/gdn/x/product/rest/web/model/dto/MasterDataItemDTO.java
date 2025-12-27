package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.InventoryType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataItemDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String skuCode;
  private String generatedItemName;
  private String upcCode;
  private boolean isActivated;
  private boolean isViewable;
  private String hash;
  private int dangerousLevel;
  private InventoryType inventoryType;
  private List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValues;
  private List<MasterDataItemImageDTO> masterDataItemImages;
  private double itemDeliveryWeight;
  private double itemHeight;
  private double itemLength;
  private double itemWeight;
  private double itemWidth;
  private String productCode;

  public MasterDataItemDTO() {

  }

  public MasterDataItemDTO(String skuCode, String generatedItemName, String upcCode,
      boolean isActivated, boolean isViewable, String hash, int dangerousLevel,
      InventoryType inventoryType,
      List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValues,
      List<MasterDataItemImageDTO> masterDataItemImages, double itemDeliveryWeight,
      double itemHeight, double itemLength, double itemWeight, double itemWidth, String productCode) {
    super();
    this.skuCode = skuCode;
    this.generatedItemName = generatedItemName;
    this.upcCode = upcCode;
    this.isActivated = isActivated;
    this.isViewable = isViewable;
    this.hash = hash;
    this.dangerousLevel = dangerousLevel;
    this.inventoryType = inventoryType;
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
    return this.dangerousLevel;
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

  public double getItemDeliveryWeight() {
    return this.itemDeliveryWeight;
  }

  public double getItemHeight() {
    return this.itemHeight;
  }

  public double getItemLength() {
    return this.itemLength;
  }

  public double getItemWeight() {
    return this.itemWeight;
  }

  public double getItemWidth() {
    return this.itemWidth;
  }

  public List<MasterDataItemAttributeValueDTO> getMasterDataItemAttributeValues() {
    return this.masterDataItemAttributeValues;
  }

  public List<MasterDataItemImageDTO> getMasterDataItemImages() {
    return this.masterDataItemImages;
  }

  public String getProductCode() {
    return productCode;
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

  public void setItemDeliveryWeight(double itemDeliveryWeight) {
    this.itemDeliveryWeight = itemDeliveryWeight;
  }

  public void setItemHeight(double itemHeight) {
    this.itemHeight = itemHeight;
  }

  public void setItemLength(double itemLength) {
    this.itemLength = itemLength;
  }

  public void setItemWeight(double itemWeight) {
    this.itemWeight = itemWeight;
  }

  public void setItemWidth(double itemWidth) {
    this.itemWidth = itemWidth;
  }

  public void setMasterDataItemAttributeValues(
      List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValues) {
    this.masterDataItemAttributeValues = masterDataItemAttributeValues;
  }

  public void setMasterDataItemImages(List<MasterDataItemImageDTO> masterDataItemImages) {
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
            "MasterDataItemDTO [skuCode=%s, generatedItemName=%s, upcCode=%s, isActivated=%s, isViewable=%s, hash=%s, dangerousLevel=%s, inventoryType=%s, masterDataItemAttributeValues=%s, masterDataItemImages=%s, itemDeliveryWeight=%s, itemHeight=%s, itemLength=%s, itemWeight=%s, itemWidth=%s, productCode=%s, toString()=%s]",
            skuCode, generatedItemName, upcCode, isActivated, isViewable, hash, dangerousLevel,
            inventoryType, masterDataItemAttributeValues, masterDataItemImages, itemDeliveryWeight,
            itemHeight, itemLength, itemWeight, itemWidth, productCode, super.toString());
  }

}
