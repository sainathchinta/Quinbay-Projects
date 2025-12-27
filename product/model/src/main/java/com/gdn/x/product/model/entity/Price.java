package com.gdn.x.product.model.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import com.gdn.x.product.enums.ProductFieldNames;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.Field;

public class Price implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;

  @Field(value = ProductFieldNames.CURRENCY)
  private String currency;

  @Field(value = ProductFieldNames.OFFER_PRICE)
  private double offerPrice;

  @Field(value = ProductFieldNames.LIST_PRICE)
  private double listPrice;

  @Field(value = ProductFieldNames.CHANNEL)
  private String channel;

  @Field(value = ProductFieldNames.DISCOUNT_PRICE)
  private List<DiscountPrice> listOfDiscountPrices = new ArrayList<DiscountPrice>();

  @Field(value = ProductFieldNames.MERCHANT_DISCOUNT_PRICE)
  private DiscountPrice merchantPromoDiscountPrice;

  @Field(value = ProductFieldNames.LAST_UPDATED_BY)
  private String lastUpdatedBy;

  @Field(value = ProductFieldNames.LAST_UPDATED_DATE)
  private Date lastUpdatedDate;

  @Transient
  private double originalSellingPrice;

  public Price() {

  }

  public Price(String currency, double offerPrice, double listPrice, String channel,
      String lastUpdatedBy, Date lastUpdatedDate) {
    this.currency = currency;
    this.offerPrice = Math.round(offerPrice);
    this.listPrice = Math.round(listPrice);
    this.channel = channel;
    this.lastUpdatedBy = lastUpdatedBy;
    this.lastUpdatedDate = lastUpdatedDate;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (this.getClass() != obj.getClass())
      return false;
    Price other = (Price) obj;
    if (this.channel == null) {
      if (other.channel != null)
        return false;
    } else if (!this.channel.equals(other.channel))
      return false;
    return true;
  }

  public String getChannel() {
    return this.channel;
  }

  public String getCurrency() {
    return this.currency;
  }

  public String getLastUpdatedBy() {
    return this.lastUpdatedBy;
  }

  public Date getLastUpdatedDate() {
    return this.lastUpdatedDate;
  }

  public List<DiscountPrice> getListOfDiscountPrices() {
    return this.listOfDiscountPrices;
  }


  public double getListPrice() {
    return Math.round(this.listPrice);
  }


  public double getOfferPrice() {
    return Math.round(this.offerPrice);
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this.channel == null) ? 0 : this.channel.hashCode());
    return result;
  }

  public void setChannel(String channel) {
    this.channel = channel;
  }

  public void setCurrency(String currency) {
    this.currency = currency;
  }

  public void setLastUpdatedBy(String lastUpdatedBy) {
    this.lastUpdatedBy = lastUpdatedBy;
  }

  public void setLastUpdatedDate(Date lastUpdatedDate) {
    this.lastUpdatedDate = lastUpdatedDate;
  }

  public void setListOfDiscountPrices(List<DiscountPrice> listOfDiscountPrices) {
    this.listOfDiscountPrices = listOfDiscountPrices;
  }

  public void setListPrice(double listPrice) {
    this.listPrice = Math.round(listPrice);
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = Math.round(offerPrice);
  }

  public DiscountPrice getMerchantPromoDiscountPrice() {
    return merchantPromoDiscountPrice;
  }

  public void setMerchantPromoDiscountPrice(DiscountPrice merchantPromoDiscountPrice) {
    this.merchantPromoDiscountPrice = merchantPromoDiscountPrice;
  }

  public double getOriginalSellingPrice() {
    return originalSellingPrice;
  }

  public void setOriginalSellingPrice(double originalSellingPrice) {
    this.originalSellingPrice = originalSellingPrice;
  }

  @Override public String toString() {
    return String.format(
        "Price [currency=%s, offerPrice=%s, listPrice=%s, channel=%s, listOfDiscountPrices=%s, "
            + "lastUpdatedBy=%s, lastUpdatedDate=%s, merchantPromoDiscountPrice=%s toString()=%s]",
        this.currency, this.offerPrice, this.listPrice, this.channel, this.listOfDiscountPrices,
        this.lastUpdatedBy, this.lastUpdatedDate, this.merchantPromoDiscountPrice,
        super.toString());
  }

}
