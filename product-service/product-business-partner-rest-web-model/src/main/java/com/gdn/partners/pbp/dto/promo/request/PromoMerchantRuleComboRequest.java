package com.gdn.partners.pbp.dto.promo.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoMerchantRuleComboRequest implements Serializable {
  private static final long serialVersionUID = 8144952123820617585L;

  private boolean mainSku;
  private String itemSku;
  private int qty;
  private double discPercentage;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public double getDiscPercentage() {
    return this.discPercentage;
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
        "PromoMerchantRuleComboRequest [mainSku=%s, itemSku=%s, qty=%s, discPercentage=%s, toString()=%s]",
        this.mainSku, this.itemSku, this.qty, this.discPercentage, super.toString());
  }
}
