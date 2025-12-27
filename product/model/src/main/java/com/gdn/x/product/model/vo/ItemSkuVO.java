package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;

public class ItemSkuVO implements Serializable {
  private static final long serialVersionUID = -1993753140309084210L;

  private String itemSku;
  private boolean instantPickup;
  private String businessPartnerCode;
  private String pickupPointCode;

  public ItemSkuVO() {
  }

  public ItemSkuVO(String itemSku, boolean instantPickup, String businessPartnerCode, String pickupPointCode) {
    this.itemSku = itemSku;
    this.instantPickup = instantPickup;
    this.businessPartnerCode = businessPartnerCode;
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public static long getSerialVersionUID() {
    return serialVersionUID;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public boolean isInstantPickup() {
    return instantPickup;
  }

  public void setInstantPickup(boolean instantPickup) {
    this.instantPickup = instantPickup;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public String toString() {
    return String.format(
        "ItemSkuVO [itemSku=%s, instantPickup=%s, businessPartnerCode=%s, pickupPointCode=%s]",
        this.itemSku, this.instantPickup, this.businessPartnerCode, this.pickupPointCode);
  }
}
