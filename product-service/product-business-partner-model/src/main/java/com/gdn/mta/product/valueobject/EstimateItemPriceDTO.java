package com.gdn.mta.product.valueobject;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EstimateItemPriceDTO {

  private static final long serialVersionUID = -1303206523907225622L;
  private static final Double OFFER_PRICE_NOT_AVAILABLE = -1d;
  private static final Double NORMAL_PRICE_NOT_AVAILABLE = -1d;
  private double normalPrice = NORMAL_PRICE_NOT_AVAILABLE;
  private double offerPrice = OFFER_PRICE_NOT_AVAILABLE;
  public static final Double ESTIMATED_PRICE_NOT_AVAILABLE = -1d;

  public double getNormalPrice() {
    return normalPrice;
  }

  public void setNormalPrice(double normalPrice) {
    this.normalPrice = normalPrice;
  }

  public double getOfferPrice() {
    return offerPrice;
  }

  public void setOfferPrice(double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public boolean isOfferPriceEstimated() {
    return !OFFER_PRICE_NOT_AVAILABLE.equals(offerPrice);
  }

  public boolean isNormalPriceEstimated() {
    return !NORMAL_PRICE_NOT_AVAILABLE.equals(normalPrice);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("EstimateItemPriceDTO{");
    sb.append("normalPrice=").append(normalPrice);
    sb.append(", offerPrice=").append(offerPrice);
    sb.append('}');
    return sb.toString();
  }
}
