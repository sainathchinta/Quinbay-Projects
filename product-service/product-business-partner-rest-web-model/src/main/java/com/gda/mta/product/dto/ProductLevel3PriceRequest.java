package com.gda.mta.product.dto;

import java.util.Date;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3PriceRequest extends BaseRequest{
  
  private static final long serialVersionUID = 4004153660814063262L;
  private String channelId;
  private Double price;
  private Double salePrice;
  private Double discountAmount;
  private Date discountStartDate;
  private Date discountEndDate;
  private String promotionName;
  
  public ProductLevel3PriceRequest() {
  	// do nothing
  }

  public ProductLevel3PriceRequest(String channelId, Double price, Double salePrice,
      Double discountAmount, Date discountStartDate, Date discountEndDate, String promotionName) {
    super();
    this.channelId = channelId;
    this.price = price;
    this.salePrice = salePrice;
    this.discountAmount = discountAmount;
    this.discountStartDate = discountStartDate;
    this.discountEndDate = discountEndDate;
    this.promotionName = promotionName;
  }

  public String getChannelId() {
    return channelId;
  }

  public void setChannelId(String channelId) {
    this.channelId = channelId;
  }

  public Double getPrice() {
    return price;
  }

  public void setPrice(Double price) {
    this.price = price;
  }

  public Double getSalePrice() {
    return salePrice;
  }

  public void setSalePrice(Double salePrice) {
    this.salePrice = salePrice;
  }

  public Double getDiscountAmount() {
    return discountAmount;
  }

  public void setDiscountAmount(Double discountAmount) {
    this.discountAmount = discountAmount;
  }

  public Date getDiscountStartDate() {
    return discountStartDate;
  }

  public void setDiscountStartDate(Date discountStartDate) {
    this.discountStartDate = discountStartDate;
  }

  public Date getDiscountEndDate() {
    return discountEndDate;
  }

  public void setDiscountEndDate(Date discountEndDate) {
    this.discountEndDate = discountEndDate;
  }

  public String getPromotionName() {
    return promotionName;
  }

  public void setPromotionName(String promotionName) {
    this.promotionName = promotionName;
  }

  public static long getSerialversionuid() {
    return serialVersionUID;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3PriceRequest [channelId=%s, price=%s, salePrice=%s, discountAmount=%s, discountStartDate=%s, discountEndDate=%s, promotionName=%s, getChannelId()=%s, getPrice()=%s, getSalePrice()=%s, getDiscountAmount()=%s, getDiscountStartDate()=%s, getDiscountEndDate()=%s, getPromotionName()=%s]",
            channelId, price, salePrice, discountAmount, discountStartDate, discountEndDate,
            promotionName, getChannelId(), getPrice(), getSalePrice(), getDiscountAmount(),
            getDiscountStartDate(), getDiscountEndDate(), getPromotionName());
  }

  
}
