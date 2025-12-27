package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
public class FailedOfflineItemResponse implements Serializable {

  private static final long serialVersionUID = -4490002695464885230L;
  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;
  private String externalPickupPointCode;
  private String errorCode;

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

  public String getErrorCode() {
    return errorCode;
  }

  public void setErrorCode(String errorCode) {
    this.errorCode = errorCode;
  }

  @Override
  public String toString() {
    return "FailedOfflineItemResponse{" + "itemSku=" + itemSku
        + ",merchantSku=" + merchantSku
        + ",pickupPointCode=" + pickupPointCode
        + ", externalPickupPointCode=" + externalPickupPointCode
        + ", errorCode=" + errorCode + '}';
  }
}
