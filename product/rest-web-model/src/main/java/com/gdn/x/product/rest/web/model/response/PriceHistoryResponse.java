package com.gdn.x.product.rest.web.model.response;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PriceHistoryResponse extends BaseResponse {

  private static final long serialVersionUID = 1L;

  private String itemSku;
  private String channel;
  private String currency;
  private double offerPrice;

  public PriceHistoryResponse() {

  }

  public PriceHistoryResponse(String itemSku, String channel, String currency, double offerPrice) {
    super();
    this.itemSku = itemSku;
    this.channel = channel;
    this.currency = currency;
    this.offerPrice = offerPrice;
  }


  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getChannel() {
    return this.channel;
  }

  public String getCurrency() {
    return this.currency;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public double getOfferPrice() {
    return this.offerPrice;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setChannel(String channel) {
    this.channel = channel;
  }

  public void setCurrency(String currency) {
    this.currency = currency;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  @Override
  public String toString() {
    return String.format(
        "PriceHistoryResponse [itemSku=%s, channel=%s, currency=%s, offerPrice=%s, toString()=%s]",
        this.itemSku, this.channel, this.currency, this.offerPrice, super.toString());
  }
}
