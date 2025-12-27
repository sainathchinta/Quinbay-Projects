package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class ComboItemVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private int quantity;
  private double discountPercentage;
  private boolean mainSku;
  private String productName;
  private List<MasterDataItemAttributeVO> masterDataItemAttributeValues = new ArrayList<>();
  private String imageUrl;
  private String productSku;
  private String productCode;
  private String itemSku;
  private String itemCode;
  private double listPrice;
  private double offerPrice;
  private double productAdjustmentPrice;
  private double finalPrice;
  private String currency;
  private boolean buyable;
  private String brand;
  private String merchantCode;
  private ItemCatalogVO masterCatalog;
  private List<ItemCatalogVO> salesCatalogs = new ArrayList<>();
  private String pristineId;

  public ComboItemVO() {
  }

  public ComboItemVO(int quantity, double discountPercentage, boolean mainSku, String productName,
      List<MasterDataItemAttributeVO> masterDataItemAttributeValues, String imageUrl,
      String productSku, String productCode, String itemSku, String itemCode, double listPrice,
      double offerPrice, double productAdjustmentPrice, double finalPrice, String currency,
      boolean buyable, String brand, String merchantCode, ItemCatalogVO masterCatalog,
      List<ItemCatalogVO> salesCatalogs, String pristineId) {
    this.quantity = quantity;
    this.discountPercentage = discountPercentage;
    this.mainSku = mainSku;
    this.productName = productName;
    this.masterDataItemAttributeValues = masterDataItemAttributeValues;
    this.imageUrl = imageUrl;
    this.productSku = productSku;
    this.productCode = productCode;
    this.itemSku = itemSku;
    this.itemCode = itemCode;
    this.listPrice = listPrice;
    this.offerPrice = offerPrice;
    this.productAdjustmentPrice = productAdjustmentPrice;
    this.finalPrice = finalPrice;
    this.currency = currency;
    this.buyable = buyable;
    this.brand = brand;
    this.merchantCode = merchantCode;
    this.masterCatalog = masterCatalog;
    this.salesCatalogs = salesCatalogs;
    this.pristineId = pristineId;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBrand() {
    return brand;
  }

  public String getCurrency() {
    return currency;
  }

  public double getDiscountPercentage() {
    return discountPercentage;
  }

  public double getFinalPrice() {
    return finalPrice;
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

  public ItemCatalogVO getMasterCatalog() {
    return masterCatalog;
  }

  public List<MasterDataItemAttributeVO> getMasterDataItemAttributeValues() {
    return masterDataItemAttributeValues;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public double getOfferPrice() {
    return offerPrice;
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

  public int getQuantity() {
    return quantity;
  }

  public List<ItemCatalogVO> getSalesCatalogs() {
    return salesCatalogs;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isBuyable() {
    return buyable;
  }

  public boolean isMainSku() {
    return mainSku;
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

  public void setDiscountPercentage(double discountPercentage) {
    this.discountPercentage = discountPercentage;
  }

  public void setFinalPrice(double finalPrice) {
    this.finalPrice = finalPrice;
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

  public void setMainSku(boolean mainSku) {
    this.mainSku = mainSku;
  }

  public void setMasterCatalog(ItemCatalogVO masterCatalog) {
    this.masterCatalog = masterCatalog;
  }

  public void setMasterDataItemAttributeValues(
      List<MasterDataItemAttributeVO> masterDataItemAttributeValues) {
    this.masterDataItemAttributeValues = masterDataItemAttributeValues;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
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

  public void setQuantity(int quantity) {
    this.quantity = quantity;
  }

  public void setSalesCatalogs(List<ItemCatalogVO> salesCatalogs) {
    this.salesCatalogs = salesCatalogs;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ComboItemVO{");
    sb.append("quantity=").append(quantity);
    sb.append(", discountPercentage=").append(discountPercentage);
    sb.append(", mainSku=").append(mainSku);
    sb.append(", productName='").append(productName).append('\'');
    sb.append(", masterDataItemAttributeValues=").append(masterDataItemAttributeValues);
    sb.append(", imageUrl='").append(imageUrl).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", productCode='").append(productCode).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
    sb.append(", listPrice=").append(listPrice);
    sb.append(", offerPrice=").append(offerPrice);
    sb.append(", productAdjustmentPrice=").append(productAdjustmentPrice);
    sb.append(", finalPrice=").append(finalPrice);
    sb.append(", currency='").append(currency).append('\'');
    sb.append(", buyable=").append(buyable);
    sb.append(", brand='").append(brand).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", masterCatalog=").append(masterCatalog);
    sb.append(", salesCatalogs=").append(salesCatalogs);
    sb.append(", pristineId='").append(pristineId).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
