package com.gdn.partners.pbp.dto.promo.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoMerchantInfoItemResponse implements Serializable {
  private static final long serialVersionUID = 3611943779572142656L;

  private String itemSku;
  private String itemCode;
  private String itemName;
  private String itemImageLocationPath;
  private double itemPrice;
  private double itemSalePrice;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemCode() {
    return this.itemCode;
  }

  public String getItemImageLocationPath() {
    return this.itemImageLocationPath;
  }

  public String getItemName() {
    return this.itemName;
  }

  public double getItemPrice() {
    return this.itemPrice;
  }

  public double getItemSalePrice() {
    return this.itemSalePrice;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemImageLocationPath(String itemImageLocationPath) {
    this.itemImageLocationPath = itemImageLocationPath;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public void setItemPrice(double itemPrice) {
    this.itemPrice = itemPrice;
  }

  public void setItemSalePrice(double itemSalePrice) {
    this.itemSalePrice = itemSalePrice;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  @Override
  public String toString() {
    return String.format(
        "PromoMerchantInfoItemResponse [itemSku=%s, itemCode=%s, itemName=%s, itemImageLocationPath=%s, itemPrice=%s, itemSalePrice=%s, toString()=%s]",
        this.itemSku, this.itemCode, this.itemName, this.itemImageLocationPath, this.itemPrice,
        this.itemSalePrice, super.toString());
  }
}
