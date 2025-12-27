package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

public class OfflineItemPriceVO {

  private String pickupPointCode;
  private double listPrice;
  private double offerPrice;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public double getListPrice() {
    return listPrice;
  }

  public void setListPrice(double listPrice) {
    this.listPrice = listPrice;
  }

  public double getOfferPrice() {
    return offerPrice;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  @Override
  public String toString() {
    return "OfflineItemPriceVO{" + "pickupPointCode='" + pickupPointCode + '\'' + ", listPrice="
        + listPrice + '\'' + ", offerPrice=" + offerPrice + '}';
  }
}
