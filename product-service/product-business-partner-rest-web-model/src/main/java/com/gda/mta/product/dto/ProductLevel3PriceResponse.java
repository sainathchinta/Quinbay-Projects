package com.gda.mta.product.dto;

import java.util.Date;

import com.gdn.common.web.base.BaseResponse;

public class ProductLevel3PriceResponse extends BaseResponse {

  private static final long serialVersionUID = 4672959842869658331L;
  private String channelId;
  private Double price;
  private Double salePrice;
  private Double discountAmount;
  private Date discountStartDate;
  private Date discountEndDate;
  private String promotionName;

  public ProductLevel3PriceResponse() {
    // do nothing
  }

  public ProductLevel3PriceResponse(String channelId, Double price, Double salePrice) {
    super();
    this.channelId = channelId;
    this.price = price;
    this.salePrice = salePrice;
  }

  public ProductLevel3PriceResponse(String channelId, Double price, Double salePrice,
      Double discountAmount, Date discountStartDate, Date discountEndDate, String promotionName) {
    this(channelId, price, salePrice);
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

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3PriceResponse [channelId=%s, price=%s, salePrice=%s, discountAmount=%s, discountStartDate=%s, discountEndDate=%s, promotionName=%s, getChannelId()=%s, getPrice()=%s, getSalePrice()=%s, getDiscountAmount()=%s, getDiscountStartDate()=%s, getDiscountEndDate()=%s, getPromotionName()=%s]",
            channelId, price, salePrice, discountAmount, discountStartDate, discountEndDate,
            promotionName, getChannelId(), getPrice(), getSalePrice(), getDiscountAmount(),
            getDiscountStartDate(), getDiscountEndDate(), getPromotionName());
  }

}