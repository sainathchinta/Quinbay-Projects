package com.gdn.x.product.model.entity;

import java.util.Date;

import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ProductFieldNames;

@Document(collection = PriceHistory.DOCUMENT_NAME)
public class PriceHistory extends GdnBaseMongoEntity {

  private static final long serialVersionUID = 1L;

  public static final String DOCUMENT_NAME = "prd_price_history";

  @Field(value = ProductFieldNames.ITEM_SKU)
  private String itemSku;

  @Field(value = ProductFieldNames.CHANNEL)
  private String channel;

  @Field(value = ProductFieldNames.CURRENCY)
  private String currency;

  @Field(value = ProductFieldNames.OFFER_PRICE)
  private double offerPrice;

  @Field(value = ProductFieldNames.LIST_PRICE)
  private double listPrice;

  @Field(value = ProductFieldNames.LAST_UPDATED_BY)
  private String lastUpdatedBy;

  @Field(value = ProductFieldNames.LAST_UPDATED_DATE)
  private Date lastUpdatedDate;

  public PriceHistory() {

  }

  public PriceHistory(String itemSku, String channel, String currency, double offerPrice,
      double listPrice, String lastUpdatedBy, Date lastUpdatedDate) {
    this.itemSku = itemSku;
    this.channel = channel;
    this.currency = currency;
    this.offerPrice = offerPrice;
    this.listPrice = listPrice;
    this.lastUpdatedBy = lastUpdatedBy;
    this.lastUpdatedDate = lastUpdatedDate;
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

  public String getLastUpdatedBy() {
    return lastUpdatedBy;
  }

  public Date getLastUpdatedDate() {
    return lastUpdatedDate;
  }

  public double getListPrice() {
    return listPrice;
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

  public void setLastUpdatedBy(String lastUpdatedBy) {
    this.lastUpdatedBy = lastUpdatedBy;
  }

  public void setLastUpdatedDate(Date lastUpdatedDate) {
    this.lastUpdatedDate = lastUpdatedDate;
  }

  public void setListPrice(double listPrice) {
    this.listPrice = listPrice;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  @Override
  public String toString() {
    return String
        .format(
            "PriceHistory [itemSku=%s, channel=%s, currency=%s, offerPrice=%s, listPrice=%s, lastUpdatedBy=%s, lastUpdatedDate=%s, toString()=%s]",
            itemSku, channel, currency, offerPrice, listPrice, lastUpdatedBy, lastUpdatedDate,
            super.toString());
  }

}
