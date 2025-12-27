package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.InventoryType;


@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterDataItemInfo implements Serializable {

  private static final long serialVersionUID = -9173345155071854170L;

  private String skuCode;
  private String generatedItemName;
  private String upcCode;
  private int dangerousLevel;
  private InventoryType inventoryType;
  private List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValues;
  private List<MasterDataItemImageDTO> masterDataItemImages;
  private double itemDeliveryWeight;

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public int getDangerousLevel() {
    return dangerousLevel;
  }

  public String getGeneratedItemName() {
    return generatedItemName;
  }

  public InventoryType getInventoryType() {
    return inventoryType;
  }

  public double getItemDeliveryWeight() {
    return itemDeliveryWeight;
  }

  public List<MasterDataItemAttributeValueDTO> getMasterDataItemAttributeValues() {
    return masterDataItemAttributeValues;
  }

  public List<MasterDataItemImageDTO> getMasterDataItemImages() {
    return masterDataItemImages;
  }

  public String getSkuCode() {
    return skuCode;
  }

  public String getUpcCode() {
    return upcCode;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setDangerousLevel(int dangerousLevel) {
    this.dangerousLevel = dangerousLevel;
  }

  public void setGeneratedItemName(String generatedItemName) {
    this.generatedItemName = generatedItemName;
  }

  public void setInventoryType(InventoryType inventoryType) {
    this.inventoryType = inventoryType;
  }

  public void setItemDeliveryWeight(double itemDeliveryWeight) {
    this.itemDeliveryWeight = itemDeliveryWeight;
  }

  public void setMasterDataItemAttributeValues(
      List<MasterDataItemAttributeValueDTO> masterDataItemAttributeValues) {
    this.masterDataItemAttributeValues = masterDataItemAttributeValues;
  }

  public void setMasterDataItemImages(List<MasterDataItemImageDTO> masterDataItemImages) {
    this.masterDataItemImages = masterDataItemImages;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }

  public void setUpcCode(String upcCode) {
    this.upcCode = upcCode;
  }

  @Override
  public String toString() {
    return String.format(
        "MasterDataItemInfo [skuCode=%s, generatedItemName=%s, upcCode=%s, dangerousLevel=%s, inventoryType=%s, masterDataItemAttributeValues=%s, masterDataItemImages=%s, itemDeliveryWeight=%s, toString()=%s]",
        skuCode, generatedItemName, upcCode, dangerousLevel, inventoryType,
        masterDataItemAttributeValues, masterDataItemImages, itemDeliveryWeight, super.toString());
  }

}
