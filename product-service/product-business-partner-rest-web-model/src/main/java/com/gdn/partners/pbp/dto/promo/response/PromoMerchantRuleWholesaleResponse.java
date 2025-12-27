package com.gdn.partners.pbp.dto.promo.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoMerchantRuleWholesaleResponse implements Serializable {
  private static final long serialVersionUID = -827484661768797892L;

  private int minQty;
  private int maxQty;
  private double discPercentage;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public double getDiscPercentage() {
    return this.discPercentage;
  }

  public int getMaxQty() {
    return this.maxQty;
  }

  public int getMinQty() {
    return this.minQty;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setDiscPercentage(double discPercentage) {
    this.discPercentage = discPercentage;
  }

  public void setMaxQty(int maxQty) {
    this.maxQty = maxQty;
  }

  public void setMinQty(int minQty) {
    this.minQty = minQty;
  }

  @Override
  public String toString() {
    return String.format(
        "PromoMerchantRuleWholesaleResponse [minQty=%s, maxQty=%s, discPercentage=%s, toString()=%s]",
        this.minQty, this.maxQty, this.discPercentage, super.toString());
  }
}
