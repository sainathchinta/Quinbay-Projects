package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnBaseBuilder;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;


@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPriceResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = -7890811996672804424L;

  public static final class ItemPriceResponseBuilder implements GdnBaseBuilder<ItemPriceResponse> {

    private String itemSku;
    private double offerPrice;
    private double listPrice;
    private boolean buyable;
    private String pickupPointCode;
    private String merchantCode;

    public ItemPriceResponseBuilder() {}

    @Override
    public ItemPriceResponse build() {
      return new ItemPriceResponse(this);
    }

    public ItemPriceResponseBuilder setBuyable(boolean buyable) {
      this.buyable = buyable;
      return this;
    }

    public ItemPriceResponseBuilder setItemSku(String itemSku) {
      this.itemSku = itemSku;
      return this;
    }

    public ItemPriceResponseBuilder setListPrice(double listPrice) {
      this.listPrice = listPrice;
      return this;
    }

    public ItemPriceResponseBuilder setOfferPrice(double offerPrice) {
      this.offerPrice = offerPrice;
      return this;
    }

    public ItemPriceResponseBuilder setPickupPointCode(String pickupPointCode) {
      this.pickupPointCode = pickupPointCode;
      return this;
    }

    public ItemPriceResponseBuilder setMerchantCode(String merchantCode) {
      this.merchantCode = merchantCode;
      return this;
    }

    @Override
    public String toString() {
      return String.format(
          "ItemPriceResponseBuilder [itemSku=%s, offerPrice=%s, listPrice=%s, buyable=%s, pickupPointCode=%s, merchantCode=%s, toString()=%s]",
          this.itemSku, this.offerPrice, this.listPrice, this.buyable, this.pickupPointCode, this.merchantCode, super.toString());
    }

  }

  private String itemSku;
  private double offerPrice;
  private double listPrice;
  private boolean buyable;
  private String pickupPointCode;
  private String merchantCode;

  public ItemPriceResponse() {}

  public ItemPriceResponse(ItemPriceResponseBuilder itemPriceResponseBuilder) {
    this.itemSku = itemPriceResponseBuilder.itemSku;
    this.listPrice = itemPriceResponseBuilder.listPrice;
    this.offerPrice = itemPriceResponseBuilder.offerPrice;
    this.merchantCode = itemPriceResponseBuilder.merchantCode;
    this.buyable = itemPriceResponseBuilder.buyable;
    this.pickupPointCode = itemPriceResponseBuilder.pickupPointCode;
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

  public void setBuyable(boolean buyable) {
    this.buyable = buyable;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemPriceResponse{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", offerPrice=").append(offerPrice);
    sb.append(", listPrice=").append(listPrice);
    sb.append(", buyable=").append(buyable);
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
