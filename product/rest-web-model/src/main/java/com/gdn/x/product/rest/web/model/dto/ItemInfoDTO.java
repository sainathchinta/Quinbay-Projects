package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.WholesaleRule;

import java.io.Serializable;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemInfoDTO implements Serializable {

  private static final long serialVersionUID = 8255100224799917673L;
  private String productName;
  private String imageUrl;
  private String productSku;
  private String itemSku;
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
  private List<ProductAttributeDetailDTO> masterDataItemAttributes;
  private List<WholesaleRule> wholesaleRules;
  private ItemCatalogDTO masterCatalog;
  private List<ItemCatalogDTO> salesCatalogs;
  private List<OfflineItemDetailDTO> offlineItems;

  public ItemInfoDTO() {
  }

  public ItemInfoDTO(String productName, String imageUrl, String productSku, String itemSku,
      String itemCode, String productCode, double listPrice, double offerPrice,
      double productAdjustmentPrice, String currency, boolean buyable, String pristineId,
      String brand, String merchantCode, List<ProductAttributeDetailDTO> masterDataItemAttributes,
      List<WholesaleRule> wholesaleRules, ItemCatalogDTO masterCatalog,
      List<ItemCatalogDTO> salesCatalogs) {
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
    return super.equals(obj);
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

  public double getListPrice() {
    return listPrice;
  }

  public ItemCatalogDTO getMasterCatalog() {
    return masterCatalog;
  }

  public List<ProductAttributeDetailDTO> getMasterDataItemAttributes() {
    return masterDataItemAttributes;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public double getOfferPrice() {
    return offerPrice;
  }

  public List<OfflineItemDetailDTO> getOfflineItems() {
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

  public List<ItemCatalogDTO> getSalesCatalogs() {
    return salesCatalogs;
  }

  public List<WholesaleRule> getWholesaleRules() {
    return wholesaleRules;
  }

  @Override
  public int hashCode() {
    return super.hashCode();
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

  public void setListPrice(double listPrice) {
    this.listPrice = listPrice;
  }

  public void setMasterCatalog(ItemCatalogDTO masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public void setMasterDataItemAttributes(
      List<ProductAttributeDetailDTO> masterDataItemAttributes) {
    this.masterDataItemAttributes = masterDataItemAttributes;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public void setOfflineItems(List<OfflineItemDetailDTO> offlineItems) {
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

  public void setSalesCatalogs(List<ItemCatalogDTO> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  public void setWholesaleRules(List<WholesaleRule> wholesaleRules) {
    this.wholesaleRules = wholesaleRules;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemInfoDTO{");
    sb.append("productName='").append(productName).append('\'');
    sb.append(", imageUrl='").append(imageUrl).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
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
