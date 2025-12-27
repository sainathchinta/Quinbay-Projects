package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleItemResponse extends BaseResponse {

  public static final class SimpleItemResponseWebBuilder implements
      GdnBaseBuilder<SimpleItemResponse> {

    private String itemSku;
    private double offerPrice;
    private boolean buyable;
    private boolean discoverable;
    private double listPrice;
    private String pickupPointCode;

    @Override
    public SimpleItemResponse build() {
      return new SimpleItemResponse(this);
    }

    public SimpleItemResponseWebBuilder setBuyable(boolean buyable) {
      this.buyable = buyable;
      return this;
    }

    public SimpleItemResponseWebBuilder setDiscoverable(boolean discoverable) {
      this.discoverable = discoverable;
      return this;
    }

    public SimpleItemResponseWebBuilder setItemSku(String itemSku) {
      this.itemSku = itemSku;
      return this;
    }

    public SimpleItemResponseWebBuilder setListPrice(double listPrice) {
      this.listPrice = listPrice;
      return this;
    }

    public SimpleItemResponseWebBuilder setOfferPrice(double offerPrice) {
      this.offerPrice = offerPrice;
      return this;
    }

    public SimpleItemResponseWebBuilder setPickupPointCode(String pickupPointCode) {
      this.pickupPointCode = pickupPointCode;
      return this;
    }

    @Override
    public String toString() {
      return String
          .format(
              "SimpleItemResponseWebBuilder [itemSku=%s, offerPrice=%s, buyable=%s, discoverable=%s, listPrice=%s, pickupPointCode=%s, toString()=%s]",
              this.itemSku, this.offerPrice, this.buyable, this.discoverable, this.listPrice, this.pickupPointCode, super.toString());
    }

  }

  private static final long serialVersionUID = 1L;

  private String itemSku;
  private double offerPrice;
  private boolean buyable;
  private boolean discoverable;
  private double listPrice;
  private String pickupPointCode;

  public SimpleItemResponse() {}

  public SimpleItemResponse(SimpleItemResponseWebBuilder builder) {
    this.itemSku = builder.itemSku;
    this.offerPrice = builder.offerPrice;
    this.listPrice = builder.listPrice;
    this.buyable = builder.buyable;
    this.discoverable = builder.discoverable;
    this.pickupPointCode = builder.pickupPointCode;
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

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isBuyable() {
    return this.buyable;
  }

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
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

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public String toString() {
    return String.format(
        "SimpleItemResponse [itemSku=%s, offerPrice=%s, buyable=%s, discoverable=%s, listPrice=%s, pickupPointCode=%s, toString()=%s]",
        this.itemSku, this.offerPrice, this.buyable, this.discoverable, this.listPrice, this.pickupPointCode,
        super.toString());
  }

}
