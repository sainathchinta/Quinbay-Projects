package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;

public class WholesaleRuleVO implements Serializable {

  private static final long serialVersionUID = 6326890882830694006L;
  private int minQuantity;
  private int maxQuantity;
  private double discountPercentage;
  private double finalPrice;

  public WholesaleRuleVO() {
  }

  public WholesaleRuleVO(int minQuantity, int maxQuantity, double discountPercentage,
      double finalPrice) {
    this.minQuantity = minQuantity;
    this.maxQuantity = maxQuantity;
    this.discountPercentage = discountPercentage;
    this.finalPrice = finalPrice;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public int getMinQuantity() {
    return minQuantity;
  }

  public void setMinQuantity(int minQuantity) {
    this.minQuantity = minQuantity;
  }

  public int getMaxQuantity() {
    return maxQuantity;
  }

  public void setMaxQuantity(int maxQuantity) {
    this.maxQuantity = maxQuantity;
  }

  public double getDiscountPercentage() {
    return discountPercentage;
  }

  public void setDiscountPercentage(double discountPercentage) {
    this.discountPercentage = discountPercentage;
  }

  public double getFinalPrice() {
    return finalPrice;
  }

  public void setFinalPrice(double finalPrice) {
    this.finalPrice = finalPrice;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("minQuantity", minQuantity)
        .append("maxQuantity", maxQuantity).append("discountPercentage", discountPercentage)
        .append("finalPrice", finalPrice).toString();
  }
}
