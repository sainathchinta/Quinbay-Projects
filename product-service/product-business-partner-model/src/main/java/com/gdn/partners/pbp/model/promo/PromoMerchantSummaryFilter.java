package com.gdn.partners.pbp.model.promo;

import java.io.Serializable;
import java.util.Set;

import com.gdn.common.base.GdnObjects;

public class PromoMerchantSummaryFilter implements Serializable {
  private static final long serialVersionUID = 38754518815285103L;

  private String businessPartnerCode;
  private String itemSku;
  private String promoId;
  private String promoType;
  private String promoName;
  private Set<String> promoStatusSet;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBusinessPartnerCode() {
    return this.businessPartnerCode;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public String getPromoId() {
    return this.promoId;
  }

  public String getPromoName() {
    return this.promoName;
  }

  public Set<String> getPromoStatusSet() {
    return this.promoStatusSet;
  }

  public String getPromoType() {
    return this.promoType;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setPromoId(String promoId) {
    this.promoId = promoId;
  }

  public void setPromoName(String promoName) {
    this.promoName = promoName;
  }

  public void setPromoStatusSet(Set<String> promoStatusSet) {
    this.promoStatusSet = promoStatusSet;
  }

  public void setPromoType(String promoType) {
    this.promoType = promoType;
  }

  @Override
  public String toString() {
    return String.format(
        "PromoMerchantSummaryFilter [businessPartnerCode=%s, itemSku=%s, promoId=%s, promoType=%s, promoName=%s, promoStatusSet=%s, toString()=%s]",
        this.businessPartnerCode, this.itemSku, this.promoId, this.promoType, this.promoName,
        this.promoStatusSet, super.toString());
  }
}
