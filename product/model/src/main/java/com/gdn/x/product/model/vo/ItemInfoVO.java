package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;
import java.util.List;

public class ItemInfoVO implements Serializable {

  private static final long serialVersionUID = -394059167481347419L;
  private String productName;
  private String imageUrl;
  private String productSku;
  private String itemSku;
  private String itemName;
  private String itemCode;
  private String productCode;
  private double listPrice;
  private double offerPrice;
  private double productAdjustmentPrice;
  private String currency;
  private boolean buyable;
  private String pristineId;
  private String brand;
  private String merchantCode;
  private List<MasterDataItemAttributeVO> masterDataItemAttributes;
  private List<WholesaleRuleVO> wholesaleRules;
  private ItemCatalogVO masterCatalog;
  private List<ItemCatalogVO> salesCatalogs;
  private List<OfflineItemDetailVo> offlineItems;

  public ItemInfoVO() {
  }

  public ItemInfoVO(String productName, String imageUrl, String productSku, String itemSku,
      String itemCode, String productCode, double listPrice, double offerPrice,
      double productAdjustmentPrice, String currency, boolean buyable, String pristineId,
      String brand, String merchantCode, List<MasterDataItemAttributeVO> masterDataItemAttributes,
      List<WholesaleRuleVO> wholesaleRules, ItemCatalogVO masterCatalog,
      List<ItemCatalogVO> salesCatalogs) {
    this.productName = productName;
    this.imageUrl = imageUrl;
    this.productSku = productSku;
    this.itemSku = itemSku;
    this.itemCode = itemCode;
    this.productCode = productCode;
    this.listPrice = listPrice;
    this.offerPrice = offerPrice;
    this.productAdjustmentPrice = productAdjustmentPrice;
    this.currency = currency;
    this.buyable = buyable;
    this.pristineId = pristineId;
    this.brand = brand;
    this.merchantCode = merchantCode;
    this.masterDataItemAttributes = masterDataItemAttributes;
    this.wholesaleRules = wholesaleRules;
    this.masterCatalog = masterCatalog;
    this.salesCatalogs = salesCatalogs;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  public String getBrand() {
    return brand;
  }

  public String getCurrency() {
    return currency;
  }

  public String getImageUrl() {
    return imageUrl;
  }

  public String getItemCode() {
    return itemCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public String getItemName() {  return itemName; }

  public double getListPrice() {
    return listPrice;
  }

  public ItemCatalogVO getMasterCatalog() {
    return masterCatalog;
  }

  public List<MasterDataItemAttributeVO> getMasterDataItemAttributes() {
    return masterDataItemAttributes;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public double getOfferPrice() {
    return offerPrice;
  }

  public List<OfflineItemDetailVo> getOfflineItems() {
    return offlineItems;
  }

  public String getPristineId() {
    return pristineId;
  }

  public double getProductAdjustmentPrice() {
    return productAdjustmentPrice;
  }

  public String getProductCode() {
    return productCode;
  }

  public String getProductName() {
    return productName;
  }

  public String getProductSku() {
    return productSku;
  }

  public List<ItemCatalogVO> getSalesCatalogs() {
    return salesCatalogs;
  }

  public List<WholesaleRuleVO> getWholesaleRules() {
    return wholesaleRules;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isBuyable() {
    return buyable;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  public void setCurrency(String currency) {
    this.currency = currency;
  }

  public void setImageUrl(String imageUrl) {
    this.imageUrl = imageUrl;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setItemName(String itemName) { this.itemName = itemName; }

  public void setListPrice(double listPrice) {
    this.listPrice = listPrice;
  }

  public void setMasterCatalog(ItemCatalogVO masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public void setMasterDataItemAttributes(
      List<MasterDataItemAttributeVO> masterDataItemAttributes) {
    this.masterDataItemAttributes = masterDataItemAttributes;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public void setOfflineItems(List<OfflineItemDetailVo> offlineItems) {
    this.offlineItems = offlineItems;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public void setProductAdjustmentPrice(double productAdjustmentPrice) {
    this.productAdjustmentPrice = productAdjustmentPrice;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setSalesCatalogs(List<ItemCatalogVO> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  public void setWholesaleRules(List<WholesaleRuleVO> wholesaleRules) {
    this.wholesaleRules = wholesaleRules;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemInfoVO{");
    sb.append("productName='").append(productName).append('\'');
    sb.append(", imageUrl='").append(imageUrl).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
    sb.append(", itemName='").append(itemName).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", listPrice=").append(listPrice);
    sb.append(", offerPrice=").append(offerPrice);
    sb.append(", productAdjustmentPrice=").append(productAdjustmentPrice);
    sb.append(", currency='").append(currency).append('\'');
    sb.append(", buyable=").append(buyable);
    sb.append(", pristineId='").append(pristineId).append('\'');
    sb.append(", brand='").append(brand).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", masterDataItemAttributes=").append(masterDataItemAttributes);
    sb.append(", wholesaleRules=").append(wholesaleRules);
    sb.append(", masterCatalog=").append(masterCatalog);
    sb.append(", salesCatalogs=").append(salesCatalogs);
    sb.append(", offlineItems=").append(offlineItems);
    sb.append('}');
    return sb.toString();
  }
}
