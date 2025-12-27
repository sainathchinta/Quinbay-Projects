package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemInstantPickupRequest implements Serializable{

  private static final long serialVersionUID = 1202926964289457148L;
  private String gdnSku;
  private String merchantCode;
  private String merchantSku;
  private String itemName;
  private String categoryCode;
  private String pickupPointCode;

  public OfflineItemInstantPickupRequest() {
  }

  public OfflineItemInstantPickupRequest(String gdnSku, String merchantCode, String merchantSku,
      String itemName, String categoryCode, String pickupPointCode) {
    this.gdnSku = gdnSku;
    this.merchantCode = merchantCode;
    this.merchantSku = merchantSku;
    this.itemName = itemName;
    this.categoryCode = categoryCode;
    this.pickupPointCode = pickupPointCode;
  }

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
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

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("OfflineItemInstantPickupRequest [");
    builder.append("gdnSku=");
    builder.append(gdnSku);
    builder.append(", merchantCode=");
    builder.append(merchantCode);
    builder.append(", merchantSku=");
    builder.append(merchantSku);
    builder.append(", itemName=");
    builder.append(itemName);
    builder.append(", categoryCode=");
    builder.append(categoryCode);
    builder.append(", pickupPointCode=");
    builder.append(pickupPointCode);
    builder.append(", toString()=");
    builder.append(super.toString());
    builder.append("]");
    return builder.toString();
  }
}
