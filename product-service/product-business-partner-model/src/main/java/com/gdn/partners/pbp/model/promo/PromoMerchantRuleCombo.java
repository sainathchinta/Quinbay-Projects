package com.gdn.partners.pbp.model.promo;

import java.io.Serializable;

import com.gdn.common.base.GdnObjects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PromoMerchantRuleCombo implements Serializable {
  private static final long serialVersionUID = -6296397311531611474L;

  private boolean mainSku;
  private String itemSku;
  private int qty;
  private double discPercentage;

  // Additional Information
  private PromoMerchantInfoItem infoItem;
  private PromoMerchantInfoInventory infoInventory;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public double getDiscPercentage() {
    return this.discPercentage;
  }

  public PromoMerchantInfoInventory getInfoInventory() {
    return this.infoInventory;
  }

  public PromoMerchantInfoItem getInfoItem() {
    return this.infoItem;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public int getQty() {
    return this.qty;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isMainSku() {
    return this.mainSku;
  }

  public void setDiscPercentage(double discPercentage) {
    this.discPercentage = discPercentage;
  }

  public void setInfoInventory(PromoMerchantInfoInventory infoInventory) {
    this.infoInventory = infoInventory;
  }

  public void setInfoItem(PromoMerchantInfoItem infoItem) {
    this.infoItem = infoItem;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setMainSku(boolean mainSku) {
    this.mainSku = mainSku;
  }

  public void setQty(int qty) {
    this.qty = qty;
  }

  @Override
  public String toString() {
    return String.format(
        "PromoMerchantRuleCombo [mainSku=%s, itemSku=%s, qty=%s, discPercentage=%s, infoItem=%s, infoInventory=%s, toString()=%s]",
        this.mainSku, this.itemSku, this.qty, this.discPercentage, this.infoItem,
        this.infoInventory, super.toString());
  }
}
