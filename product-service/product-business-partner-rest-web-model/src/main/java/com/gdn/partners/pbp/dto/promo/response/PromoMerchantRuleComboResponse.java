package com.gdn.partners.pbp.dto.promo.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoMerchantRuleComboResponse implements Serializable {
  private static final long serialVersionUID = -8705072763356185801L;

  private boolean mainSku;
  private String itemSku;
  private int qty;
  private double discPercentage;

  // Additional Information
  private PromoMerchantInfoItemResponse infoItem;
  private PromoMerchantInfoInventoryResponse infoInventory;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public double getDiscPercentage() {
    return this.discPercentage;
  }

  public PromoMerchantInfoInventoryResponse getInfoInventory() {
    return this.infoInventory;
  }

  public PromoMerchantInfoItemResponse getInfoItem() {
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

  public void setInfoInventory(PromoMerchantInfoInventoryResponse infoInventory) {
    this.infoInventory = infoInventory;
  }

  public void setInfoItem(PromoMerchantInfoItemResponse infoItem) {
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
        "PromoMerchantRuleComboResponse [mainSku=%s, itemSku=%s, qty=%s, discPercentage=%s, infoItem=%s, infoInventory=%s, toString()=%s]",
        this.mainSku, this.itemSku, this.qty, this.discPercentage, this.infoItem,
        this.infoInventory, super.toString());
  }
}
