package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;


public class Off2OnPriceVO {

  private String itemSku;

  private double offerPrice;

  private double listPrice;

  private boolean off2OnChannelActive;

  public Off2OnPriceVO() {}

  public Off2OnPriceVO(String itemSku, double offerPrice, double listPrice,
      boolean off2OnChannelActive) {
    this.itemSku = itemSku;
    this.offerPrice = offerPrice;
    this.listPrice = listPrice;
    this.off2OnChannelActive = off2OnChannelActive;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public double getListPrice() {
    return this.listPrice;
  }

  public double getOfferPrice() {
    return this.offerPrice;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isOff2OnChannelActive() {
    return this.off2OnChannelActive;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setListPrice(double listPrice) {
    this.listPrice = listPrice;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  @Override
  public String toString() {
    return String
        .format(
            "Off2OnDataResponse [itemSku=%s, offerPrice=%s, listPrice=%s, off2OnChannelActive=%s, toString()=%s]",
            this.itemSku, this.offerPrice, this.listPrice, this.off2OnChannelActive,
            super.toString());
  }

}
