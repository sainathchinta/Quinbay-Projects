package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSkuDTO extends BaseRequest {
  private static final long serialVersionUID = -2432845512891449422L;

  private String itemSku;
  private boolean instantPickup;
  private String businessPartnerCode;
  private String pickupPointCode;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public ItemSkuDTO() {
  }

  public ItemSkuDTO(String itemSku, boolean instantPickup, String businessPartnerCode, String pickupPointCode) {
    this.itemSku = itemSku;
    this.instantPickup = instantPickup;
    this.businessPartnerCode = businessPartnerCode;
    this.pickupPointCode = pickupPointCode;
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
        "ItemSkuDTO [itemSku=%s, instantPickup=%s, businessPartnerCode=%s, pickupPointCode=%s]",
        this.itemSku, this.instantPickup, this.businessPartnerCode, this.pickupPointCode);
  }
}
