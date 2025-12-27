package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemRequest extends BaseRequest {

  private static final long serialVersionUID = -6723548677223529152L;

  private String offlineItemId;
  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;
  private String externalPickupPointCode;
  private Double listPrice;
  private Double offerPrice;

  public OfflineItemRequest() {
  }

  public OfflineItemRequest(String offlineItemId, String itemSku, String merchantSku,
      String pickupPointCode, String externalPickupPointCode, Double offerPrice) {
    this.offlineItemId = offlineItemId;
    this.itemSku = itemSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.externalPickupPointCode = externalPickupPointCode;
    this.offerPrice = offerPrice;
  }

  public OfflineItemRequest(String offlineItemId, String itemSku, String merchantSku,
    String pickupPointCode, String externalPickupPointCode, Double listPrice, Double offerPrice) {
    this.offlineItemId = offlineItemId;
    this.itemSku = itemSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.externalPickupPointCode = externalPickupPointCode;
    this.listPrice = listPrice;
    this.offerPrice = offerPrice;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public String getOfflineItemId() {
    return offlineItemId;
  }

  public void setOfflineItemId(String offlineItemId) {
    this.offlineItemId = offlineItemId;
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

  public Double getListPrice() {
    return listPrice;
  }

  public void setListPrice(Double listPrice) {
    this.listPrice = listPrice;
  }

  public Double getOfferPrice() {
    return offerPrice;
  }

  public void setOfferPrice(Double offerPrice) {
    this.offerPrice = offerPrice;
  }

  @Override
  public String toString() {
    return "OfflineItemRequest{" + "offlineItemId='" + offlineItemId + '\'' + ", itemSku='"
        + itemSku + '\'' + ", merchantSku='" + merchantSku + '\'' + ", pickupPointCode='"
        + pickupPointCode + '\'' + ", externalPickupPointCode='" + externalPickupPointCode + '\''
        + ", listPrice=" + listPrice + '\'' + ", offerPrice=" + offerPrice + '}';
  }
}
