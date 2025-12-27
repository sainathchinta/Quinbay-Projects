package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
@JsonIgnoreProperties(ignoreUnknown = true)
public class DeleteOfflineItemRequest extends BaseRequest {

  private static final long serialVersionUID = -154693142582386386L;

  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;

  public DeleteOfflineItemRequest() {
  }

  public DeleteOfflineItemRequest(String itemSku, String merchantSku,
      String pickupPointCode) {
    this.itemSku = itemSku;
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
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
