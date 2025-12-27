package com.gdn.partners.pbp.dto.promo.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoMerchantInfoInventoryResponse implements Serializable {
  private static final long serialVersionUID = 782843211068649331L;

  private int stock;
  private int stockReserved;
  private boolean syncStock;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public int getStock() {
    return this.stock;
  }

  public int getStockReserved() {
    return this.stockReserved;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isSyncStock() {
    return this.syncStock;
  }

  public void setStock(int stock) {
    this.stock = stock;
  }

  public void setStockReserved(int stockReserved) {
    this.stockReserved = stockReserved;
  }

  public void setSyncStock(boolean syncStock) {
    this.syncStock = syncStock;
  }

  @Override
  public String toString() {
    return String.format(
        "PromoMerchantInfoInventoryResponse [stock=%s, stockReserved=%s, syncStock=%s, toString()=%s]",
        this.stock, this.stockReserved, this.syncStock, super.toString());
  }
}
