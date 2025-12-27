package com.gdn.partners.pbp.model.promo;

import java.io.Serializable;

import com.gdn.common.base.GdnObjects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PromoMerchantInfoInventory implements Serializable {
  private static final long serialVersionUID = 3162837302676465059L;

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
        "PromoMerchantInfoInventory [stock=%s, stockReserved=%s, syncStock=%s, toString()=%s]",
        this.stock, this.stockReserved, this.syncStock, super.toString());
  }
}
