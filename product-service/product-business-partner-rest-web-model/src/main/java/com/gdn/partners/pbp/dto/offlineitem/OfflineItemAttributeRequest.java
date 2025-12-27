package com.gdn.partners.pbp.dto.offlineitem;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemAttributeRequest implements Serializable {

  private static final long serialVersionUID = -3877150131361175614L;

  private String merchantSku;
  private String externalPickupPointCode;
  private Integer stock;
  private Double listPrice;
  private Double price;
  private String fileName;
  private String itemSku;
  private String pickupPointCode;

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
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

  public String getItemSku() { return itemSku; }

  public void setItemSku(String itemSku) { this.itemSku = itemSku; }

  public String getPickupPointCode() { return pickupPointCode; }

  public void setPickupPointCode(String pickupPointCode) { this.pickupPointCode = pickupPointCode; }

  @Override
  public String toString() {
    return "OfflineItemAttributeRequest [merchantSku=" + merchantSku + ", externalPickupPointCode="
        + externalPickupPointCode + ", stock=" + stock + ", listPrice=" + listPrice
        + ", price=" + price + ", fileName=" + fileName + ", itemSku=" + itemSku
        + ", pickupPointCode=" + pickupPointCode + "]";
  }

}
