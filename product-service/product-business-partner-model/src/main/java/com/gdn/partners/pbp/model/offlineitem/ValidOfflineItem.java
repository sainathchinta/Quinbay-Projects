package com.gdn.partners.pbp.model.offlineitem;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ValidOfflineItem implements Serializable {

  private static final long serialVersionUID = -1964472466762010247L;
  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;
  private String externalPickupPointCode;
  private Integer stock;
  private Double listPrice;
  private Double price;
  private String fileName;

  public ValidOfflineItem() {}

  public ValidOfflineItem(String itemSku, String merchantSku, String pickupPointCode,
      String externalPickupPointCode, Integer stock, Double price, String fileName) {
    this.itemSku = itemSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.externalPickupPointCode = externalPickupPointCode;
    this.stock = stock;
    this.price = price;
    this.fileName = fileName;
  }

  public ValidOfflineItem(String itemSku, String merchantSku, String pickupPointCode,
    String externalPickupPointCode, Integer stock, Double listPrice, Double price,
    String fileName) {
    this.itemSku = itemSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.externalPickupPointCode = externalPickupPointCode;
    this.stock = stock;
    this.listPrice = listPrice;
    this.price = price;
    this.fileName = fileName;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public String getExternalPickupPointCode() {
    return externalPickupPointCode;
  }

  public void setExternalPickupPointCode(String externalPickupPointCode) {
    this.externalPickupPointCode = externalPickupPointCode;
  }

  public Integer getStock() {
    return stock;
  }

  public void setStock(Integer stock) {
    this.stock = stock;
  }

  public Double getListPrice() {
    return listPrice;
  }

  public void setListPrice(Double listPrice) {
    this.listPrice = listPrice;
  }

  public Double getPrice() {
    return price;
  }

  public void setPrice(Double price) {
    this.price = price;
  }

  public String getFileName() {
    return fileName;
  }

  public void setFileName(String fileName) {
    this.fileName = fileName;
  }

  @Override
  public String toString() {
    return "ValidOfflineItem{" + "itemSku=" + itemSku + ", merchantSku=" + merchantSku
        + ", pickupPointCode=" + pickupPointCode + ", externalPickupPointCode="
        + externalPickupPointCode + ", stock=" + stock + ", listPrice=" + listPrice
        + ", price=" + price + ", fileName=" + fileName + '}';
  }
}
