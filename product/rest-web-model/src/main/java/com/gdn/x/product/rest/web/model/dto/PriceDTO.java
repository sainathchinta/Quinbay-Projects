package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.ChannelName;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PriceDTO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String currency;
  private double offerPrice;
  private double listPrice;
  private String channel = ChannelName.DEFAULT.toString();
  private List<DiscountPriceDTO> listOfDiscountPrices = new ArrayList<DiscountPriceDTO>();
  private DiscountPriceDTO merchantPromoDiscountPrice;
  private String lastUpdatedBy;
  private Date lastUpdatedDate;
  private double originalSellingPrice;

  public PriceDTO() {

  }

  public PriceDTO(String currency, double offerPrice, double listPrice, String channel,
      List<DiscountPriceDTO> listOfDiscountPrices, String lastUpdatedBy, Date lastUpdatedDate) {
    this.currency = currency;
    this.offerPrice = offerPrice;
    this.listPrice = listPrice;
    this.channel = channel;
    this.listOfDiscountPrices = listOfDiscountPrices;
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

  public String getLastUpdatedBy() {
    return this.lastUpdatedBy;
  }

  public Date getLastUpdatedDate() {
    return this.lastUpdatedDate;
  }

  public List<DiscountPriceDTO> getListOfDiscountPrices() {
    return this.listOfDiscountPrices;
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

  public void setListOfDiscountPrices(List<DiscountPriceDTO> listOfDiscountPrices) {
    this.listOfDiscountPrices = listOfDiscountPrices;
  }

  public void setListPrice(double listPrice) {
    this.listPrice = listPrice;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public DiscountPriceDTO getMerchantPromoDiscountPrice() {
    return merchantPromoDiscountPrice;
  }

  public void setMerchantPromoDiscountPrice(DiscountPriceDTO merchantPromoDiscountPrice) {
    this.merchantPromoDiscountPrice = merchantPromoDiscountPrice;
  }

  public double getOriginalSellingPrice() {
    return originalSellingPrice;
  }

  public void setOriginalSellingPrice(double originalSellingPrice) {
    this.originalSellingPrice = originalSellingPrice;
  }

  @Override
  public String toString() {
    return String
        .format(
            "PriceDTO [currency=%s, offerPrice=%s, listPrice=%s, channel=%s, listOfDiscountPrices=%s, lastUpdatedBy=%s, lastUpdatedDate=%s, merchantPromoDiscountPrice=%s, getClass()=%s, toString()=%s]",
            this.currency, this.offerPrice, this.listPrice, this.channel,
            this.listOfDiscountPrices, this.lastUpdatedBy, this.lastUpdatedDate, this.merchantPromoDiscountPrice, this.getClass(),
            super.toString());
  }
}
