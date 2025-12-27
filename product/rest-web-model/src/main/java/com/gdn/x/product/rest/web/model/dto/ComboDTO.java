package com.gdn.x.product.rest.web.model.dto;

import com.gdn.common.base.GdnObjects;

import java.io.Serializable;

public class ComboDTO implements Serializable {
  private static final long serialVersionUID = -146057928592775786L;
  private String itemSku;
  private int quantity;
  private double discountPercentage;
  private boolean mainSku;
  private double finalPrice;

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public int getQuantity() {
    return quantity;
  }

  public void setQuantity(int quantity) {
    this.quantity = quantity;
  }

  public double getDiscountPercentage() {
    return discountPercentage;
  }

  public void setDiscountPercentage(double discountPercentage) {
    this.discountPercentage = discountPercentage;
  }

  public boolean isMainSku() {
    return mainSku;
  }

  public void setMainSku(boolean mainSku) {
    this.mainSku = mainSku;
  }

  public double getFinalPrice() {
    return finalPrice;
  }

  public void setFinalPrice(double finalPrice) {
    this.finalPrice = finalPrice;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ComboDTO{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", quantity=").append(quantity);
    sb.append(", discountPercentage=").append(discountPercentage);
    sb.append(", mainSku=").append(mainSku);
    sb.append(", finalPrice=").append(finalPrice);
    sb.append('}');
    return sb.toString();
  }
}
