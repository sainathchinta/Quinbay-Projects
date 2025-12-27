package com.gdn.mta.product.entity;

import java.io.Serializable;
import java.util.Date;

import com.gdn.common.web.base.BaseResponse;

public class ProductLevel3Price extends BaseResponse implements Serializable {

  private static final long serialVersionUID = -2606589124347254689L;
  private String channelId;
  private Double price;
  private Double salePrice;
  private Double discountAmount;
  private Date discountStartDate;
  private Date discountEndDate;
  private String promotionName;

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

  public ProductLevel3Price() {
    // do nothing
  }

  public ProductLevel3Price(String channelId, Double price, Double salePrice) {
    super();
    this.channelId = channelId;
    this.price = price;
    this.salePrice = salePrice;
  }

  public ProductLevel3Price(String channelId, Double price, Double salePrice,
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

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3Price [channelId=").append(channelId).append(", price=")
        .append(price).append(", salePrice=").append(salePrice).append(", discountAmount=")
        .append(discountAmount).append(", discountStartDate=").append(discountStartDate)
        .append(", discountEndDate=").append(discountEndDate).append(", promotionName=")
        .append(promotionName).append(", getDiscountAmount()=").append(getDiscountAmount())
        .append(", getDiscountStartDate()=").append(getDiscountStartDate())
        .append(", getDiscountEndDate()=").append(getDiscountEndDate())
        .append(", getPromotionName()=").append(getPromotionName()).append(", getChannelId()=")
        .append(getChannelId()).append(", getPrice()=").append(getPrice())
        .append(", getSalePrice()=").append(getSalePrice()).append("]");
    return builder.toString();
  }


}
