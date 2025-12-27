package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.List;

public class ProductLevel3SummaryMinified implements Serializable {

  private static final long serialVersionUID = -1369511392856584708L;
  private String itemSku;
  private String skuCode;
  private String merchantSku;
  private String itemName;
  private Integer availableStockLevel2;
  private Integer reservedStockLevel2;
  private List<ProductLevel3Price> prices;
  private List<ProductLevel3ViewConfig> viewConfigs;
  private List<ProductLevel3Image> images;
  private boolean promoBundling;

  public ProductLevel3SummaryMinified() {}

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getSkuCode() {
    return skuCode;
  }

  public void setSkuCode(String skuCode) {
    this.skuCode = skuCode;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public String getItemName() {
    return itemName;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public Integer getAvailableStockLevel2() {
    return availableStockLevel2;
  }

  public void setAvailableStockLevel2(Integer availableStockLevel2) {
    this.availableStockLevel2 = availableStockLevel2;
  }

  public Integer getReservedStockLevel2() {
    return reservedStockLevel2;
  }

  public void setReservedStockLevel2(Integer reservedStockLevel2) {
    this.reservedStockLevel2 = reservedStockLevel2;
  }

  public List<ProductLevel3Price> getPrices() {
    return prices;
  }

  public void setPrices(List<ProductLevel3Price> prices) {
    this.prices = prices;
  }

  public List<ProductLevel3ViewConfig> getViewConfigs() {
    return viewConfigs;
  }

  public void setViewConfigs(List<ProductLevel3ViewConfig> viewConfigs) {
    this.viewConfigs = viewConfigs;
  }

  public List<ProductLevel3Image> getImages() {
    return images;
  }

  public void setImages(List<ProductLevel3Image> images) {
    this.images = images;
  }

  public static long getSerialversionuid() {
    return serialVersionUID;
  }

  public boolean isPromoBundling() {
    return promoBundling;
  }

  public void setPromoBundling(boolean promoBundling) {
    this.promoBundling = promoBundling;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3SummaryMinified [itemSku=").append(itemSku).append(", skuCode=")
        .append(skuCode).append(", merchantSku=").append(merchantSku).append(", itemName=")
        .append(itemName).append(", availableStockLevel2=").append(availableStockLevel2)
        .append(", reservedStockLevel2=").append(reservedStockLevel2).append(", prices=")
        .append(prices).append(", viewConfigs=").append(viewConfigs).append(", images=")
        .append(images).append(", getItemSku()=").append(getItemSku()).append(", getSkuCode()=")
        .append(getSkuCode()).append(", getMerchantSku()=").append(getMerchantSku())
        .append(", getItemName()=").append(getItemName()).append(", getAvailableStockLevel2()=")
        .append(getAvailableStockLevel2()).append(", getReservedStockLevel2()=")
        .append(getReservedStockLevel2()).append(", getPrices()=").append(getPrices())
        .append(", getViewConfigs()=").append(getViewConfigs()).append(", getImages()=")
        .append(getImages()).append("]");
    return builder.toString();
  }


}
