package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;

public class HandlingFeeRequest {

  private String itemSku;
  private int quantity;

  public HandlingFeeRequest() {}

  public HandlingFeeRequest(String orderId, int quantity) {
    this.itemSku = orderId;
    this.quantity = quantity;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
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

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setQuantity(int quantity) {
    this.quantity = quantity;
  }

  @Override
  public String toString() {
    return String.format("HandlingFeeRequest [itemSku=%s, quantity=%s, toString()=%s]", itemSku,
        quantity, super.toString());
  }

}
