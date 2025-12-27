package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
public class DeleteOfflineItemRequest implements Serializable {

  private static final long serialVersionUID = -1858200797289982393L;

  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;

  public DeleteOfflineItemRequest() {
  }

  public DeleteOfflineItemRequest(String itemSku, String merchantSku, String pickupPointCode) {
    this.itemSku = itemSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("DeleteOfflineItemRequest{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
