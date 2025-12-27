package com.gdn.x.product.model.vo;

import java.io.Serializable;

import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;

public class ItemPriceVO implements Serializable {

  public static final class ItemPriceBuilder implements GdnBaseBuilder<ItemPriceVO> {

    private String itemSku;
    private double offerPrice;
    private double listPrice;
    private boolean buyable;
    private boolean discoverable;
    private String pickupPointCode;
    private String merchantCode;

    public ItemPriceBuilder() {}

    @Override
    public ItemPriceVO build() {
      return new ItemPriceVO(this);
    }

    public ItemPriceBuilder setBuyable(boolean buyable) {
      this.buyable = buyable;
      return this;
    }

    public ItemPriceBuilder setDiscoverable(boolean discoverable) {
      this.discoverable = discoverable;
      return this;
    }

    public ItemPriceBuilder setBuyable(int buyable) {
      this.buyable = buyable == 1;
      return this;
    }

    public ItemPriceBuilder setItemSku(String itemSku) {
      this.itemSku = itemSku;
      return this;
    }

    public ItemPriceBuilder setListPrice(double listPrice) {
      this.listPrice = listPrice;
      return this;
    }

    public ItemPriceBuilder setOfferPrice(double offerPrice) {
      this.offerPrice = offerPrice;
      return this;
    }

    public ItemPriceBuilder setPickupPointCode(String pickupPointCode) {
      this.pickupPointCode = pickupPointCode;
      return this;
    }

    public ItemPriceBuilder setMerchantCode(String merchantCode) {
      this.merchantCode = merchantCode;
      return this;
    }

    @Override
    public String toString() {
      return String.format(
          "ItemPriceBuilder [itemSku=%s, offerPrice=%s, buyable=%s, discoverable=%s, pickupPointCode=%s, merchantCode=%s, toString()=%s]",
          this.itemSku, this.offerPrice, this.buyable, this.discoverable, this.pickupPointCode, this.merchantCode, super.toString());
    }

  }

  private static final long serialVersionUID = 1L;

  private String itemSku;
  private double offerPrice;
  private double listPrice;
  private boolean buyable;
  private boolean discoverable;
  private String pickupPointCode;
  private String merchantCode;

  public ItemPriceVO() {}

  public ItemPriceVO(ItemPriceBuilder builder) {
    this.itemSku = builder.itemSku;
    this.offerPrice = builder.offerPrice;
    this.setListPrice(builder.listPrice);
    this.buyable = builder.buyable;
    this.pickupPointCode = builder.pickupPointCode;
    this.merchantCode = builder.merchantCode;
    this.discoverable = builder.discoverable;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public double getListPrice() {
    return listPrice;
  }

  public double getOfferPrice() {
    return this.offerPrice;
  }

  public String getPickupPointCode(){
    return this.pickupPointCode;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isBuyable() {
    return this.buyable;
  }

  public void setBuyable(int buyable) {
    this.buyable = false;
    if (buyable == 1) {
      this.buyable = true;
    }
  }

  public boolean isDiscoverable() { return this.discoverable; }

  public void setDiscoverable(boolean discoverable) {
    this.discoverable = discoverable;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setListPrice(double listPrice) {
    this.listPrice = listPrice;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public void setPickupPointCode(String pickupPointCode){
    this.pickupPointCode = pickupPointCode;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  @Override
  public String toString() {
    return String.format(
        "ItemPriceVO [itemSku=%s, offerPrice=%s, buyable=%s, discoverable=%s, pickupPointCode=%s, merchantCode = %s, toString()=%s]",
        this.itemSku, this.offerPrice, this.buyable, this.discoverable, this.pickupPointCode, this.merchantCode, super.toString());
  }

}
