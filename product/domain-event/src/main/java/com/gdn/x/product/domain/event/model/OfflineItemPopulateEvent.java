package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemPopulateEvent extends ProductBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -4082989728754787192L;

  private String uniqueId;
  private String merchantCode;
  private String itemSku;
  private String itemCode;
  private String merchantSku;
  private String pickupPointCode;
  private String externalPickupPointCode;
  private String productSku;
  private Double listPrice;
  private Double offerPrice;
  private Boolean newData;
  private Double oldListPrice;
  private Double oldOfferPrice;
  private Boolean syncPriceAction;
  private String requestId;
  private Date updatedDate;
  private String username;
  private String clientId;
  private String pristineId;

  public OfflineItemPopulateEvent() {
  }

  public OfflineItemPopulateEvent(String uniqueId, String merchantCode, String itemSku, String merchantSku,
      String pickupPointCode, String externalPickupPointCode, String productSku,
      Double listPrice, Double offerPrice, Boolean newData, String pristineId) {
    this.uniqueId = uniqueId;
    this.merchantCode = merchantCode;
    this.itemSku = itemSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.externalPickupPointCode = externalPickupPointCode;
    this.productSku = productSku;
    this.listPrice = listPrice;
    this.offerPrice = offerPrice;
    this.newData = newData;
    this.pristineId = pristineId;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getItemCode() {

    return itemCode;
  }

  public void setItemCode(String itemCode) {

    this.itemCode = itemCode;
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

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
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

  public String getUniqueId() {
    return uniqueId;
  }

  public void setUniqueId(String uniqueId) {
    this.uniqueId = uniqueId;
  }

  public Boolean getNewData() {
    return this.newData;
  }

  public void setNewData(Boolean newData) {
    this.newData = newData;
  }

  public Double getOldListPrice() {
    return oldListPrice;
  }

  public void setOldListPrice(Double oldListPrice) {
    this.oldListPrice = oldListPrice;
  }

  public Double getOldOfferPrice() {
    return oldOfferPrice;
  }

  public void setOldOfferPrice(Double oldOfferPrice) {
    this.oldOfferPrice = oldOfferPrice;
  }

  public Boolean getSyncPriceAction() {
    return syncPriceAction;
  }

  public void setSyncPriceAction(Boolean syncPriceAction) {
    this.syncPriceAction = syncPriceAction;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public Date getUpdatedDate() {
    return updatedDate;
  }

  public void setUpdatedDate(Date updatedDate) {
    this.updatedDate = updatedDate;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getClientId() {
    return clientId;
  }

  public void setClientId(String clientId) {
    this.clientId = clientId;
  }

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }


  @Override
  public String toString() {
    return "OfflineItemPopulateEvent{" + "uniqueId='" + uniqueId + '\'' + ", merchantCode='" + merchantCode
        + '\'' + ", itemSku='" + itemSku + '\'' + ", itemCode='" + itemCode + '\''
        + ", merchantSku='" + merchantSku + '\'' + ", pickupPointCode='" + pickupPointCode + '\''
        + ", externalPickupPointCode='" + externalPickupPointCode + '\'' + ", productSku='"
        + productSku + '\'' + ", listPrice=" + listPrice + ", offerPrice=" + offerPrice
        + ", newData=" + newData + ", oldListPrice=" + oldListPrice + ", oldOfferPrice="
        + oldOfferPrice + ", syncPriceAction=" + syncPriceAction + ", requestId='" + requestId
        + '\'' + ", updatedDate=" + updatedDate + ", username='" + username + '\'' + ", clientId='"
        + clientId + '\'' + ", pristineId='" + pristineId + '\'' + '}';
  }
}
