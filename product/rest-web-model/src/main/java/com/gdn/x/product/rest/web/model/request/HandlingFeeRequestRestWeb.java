package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
@JsonIgnoreProperties(ignoreUnknown = true)
public class HandlingFeeRequestRestWeb extends BaseRequest implements Serializable {

  private static final long serialVersionUID = 1L;
  private String itemSku;
  private int quantity;

  public HandlingFeeRequestRestWeb() {}

  public HandlingFeeRequestRestWeb(String orderId, int quantity) {
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
    return String.format("HandlingFeeRequestRestWeb [itemSku=%s, quantity=%s, toString()=%s]",
        itemSku, quantity, super.toString());
  }

}
