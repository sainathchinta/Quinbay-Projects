package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;

public class ComboRuleVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private String itemSku;
  private int quantity;
  private double discountPercentage;
  private boolean mainSku;
  private double finalPrice;

  public ComboRuleVO() {
  }

  public ComboRuleVO(String itemSku, int quantity, double discountPercentage, boolean mainSku,
      double finalPrice) {
    this.itemSku = itemSku;
    this.quantity = quantity;
    this.discountPercentage = discountPercentage;
    this.mainSku = mainSku;
    this.finalPrice = finalPrice;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public double getDiscountPercentage() {
    return discountPercentage;
  }

  public double getFinalPrice() {
    return finalPrice;
  }

  public String getItemSku() {
    return itemSku;
  }

  public int getQuantity() {
    return quantity;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isMainSku() {
    return mainSku;
  }

  public void setDiscountPercentage(double discountPercentage) {
    this.discountPercentage = discountPercentage;
  }

  public void setFinalPrice(double finalPrice) {
    this.finalPrice = finalPrice;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setMainSku(boolean mainSku) {
    this.mainSku = mainSku;
  }

  public void setQuantity(int quantity) {
    this.quantity = quantity;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ComboRuleVO{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", quantity=").append(quantity);
    sb.append(", discountPercentage=").append(discountPercentage);
    sb.append(", mainSku=").append(mainSku);
    sb.append(", finalPrice=").append(finalPrice);
    sb.append('}');
    return sb.toString();
  }
}
