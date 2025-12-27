package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemPriceResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 1522321393193561705L;

  private String offlineItemId;
  private String merchantCode;
  private String itemSku;
  private String pickupPointCode;
  private Double listPrice;
  private Double offerPrice;

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

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Double getOfferPrice() {
    return offerPrice;
  }

  public void setOfferPrice(Double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public Double getListPrice() {
    return listPrice;
  }

  public void setListPrice(Double listPrice) {
    this.listPrice = listPrice;
  }

  public String getOfflineItemId() {
    return offlineItemId;
  }

  public void setOfflineItemId(String offlineItemId) {
    this.offlineItemId = offlineItemId;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    return "OfflineItemPriceResponse{" + "offlineItemId='" + offlineItemId + '\''
        + ", merchantCode='" + merchantCode + '\'' + ", itemSku='" + itemSku + '\''
        + ", pickupPointCode='" + pickupPointCode + '\'' + ", listPrice='" + listPrice + '\''
        + ", offerPrice=" + offerPrice + '}';
  }
}
