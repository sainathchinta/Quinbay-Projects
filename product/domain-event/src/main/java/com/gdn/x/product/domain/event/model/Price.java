package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Price implements Serializable {

  private static final long serialVersionUID = -246113889629482088L;

  private String currency;

  private double offerPrice;

  private double listPrice;

  private List<DiscountPrice> listOfDiscountPrices;

  private DiscountPrice merchantPromoDiscountPrice;

  private String channel;

  private String lastUpdatedBy;

  private Date lastUpdatedDate;

  public Price() {

  }

  public Price(String currency, double offerPrice, double listPrice, String channel,
      String lastUpdatedBy, Date lastUpdatedDate) {
    this.currency = currency;
    this.offerPrice = offerPrice;
    this.listPrice = listPrice;
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
    return lastUpdatedBy;
  }

  public Date getLastUpdatedDate() {
    return lastUpdatedDate;
  }

  public double getListPrice() {
    return this.listPrice;
  }

  public double getOfferPrice() {
    return this.offerPrice;
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

  public void setListPrice(double listPrice) {
    this.listPrice = listPrice;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public List<DiscountPrice> getListOfDiscountPrices() {
    return listOfDiscountPrices;
  }

  public void setListOfDiscountPrices(List<DiscountPrice> listOfDiscountPrices) {
    this.listOfDiscountPrices = listOfDiscountPrices;
  }

  public DiscountPrice getMerchantPromoDiscountPrice() {
    return merchantPromoDiscountPrice;
  }

  public void setMerchantPromoDiscountPrice(DiscountPrice merchantPromoDiscountPrice) {
    this.merchantPromoDiscountPrice = merchantPromoDiscountPrice;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("currency", currency).append("offerPrice", offerPrice)
        .append("listPrice", listPrice).append("listOfDiscountPrices", listOfDiscountPrices)
        .append("channel", channel).append("lastUpdatedBy", lastUpdatedBy)
        .append("lastUpdatedDate", lastUpdatedDate)
        .append("merchantPromoDiscountPrice", merchantPromoDiscountPrice).toString();
  }
}
