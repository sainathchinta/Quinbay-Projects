package com.gdn.partners.pbp.model.offlineitem;

import java.io.Serializable;

public class OfflineItem implements Serializable {
  private static final long serialVersionUID = 3179545455954600787L;

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
    StringBuilder builder = new StringBuilder();
    builder.append("OfflineItem [merchantSku=");
    builder.append(merchantSku);
    builder.append(", externalPickupPointCode=");
    builder.append(externalPickupPointCode);
    builder.append(", stock=");
    builder.append(stock);
    builder.append(", listPrice=");
    builder.append(listPrice);
    builder.append(", price=");
    builder.append(price);
    builder.append(", fileName=");
    builder.append(fileName);
    builder.append(", itemSku=");
    builder.append(itemSku);
    builder.append(", pickupPointCode=");
    builder.append(pickupPointCode);
    builder.append("]");
    return builder.toString();
  }
}
