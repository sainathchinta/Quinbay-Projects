package com.gdn.partners.pbp.model.offlineitem;

import com.gdn.common.base.GdnObjects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DeleteOfflineItem implements Serializable {

  private static final long serialVersionUID = -3529260106637670866L;

  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
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

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
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
    final StringBuilder sb = new StringBuilder("DeleteOfflineItem{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
