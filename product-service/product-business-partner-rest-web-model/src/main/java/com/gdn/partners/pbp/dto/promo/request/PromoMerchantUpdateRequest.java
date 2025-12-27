package com.gdn.partners.pbp.dto.promo.request;

import java.io.Serializable;
import java.util.Date;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoMerchantUpdateRequest implements Serializable {
  private static final long serialVersionUID = 2971974077075780585L;

  private String itemSku;
  private String promoId;
  private String promoType;
  private String promoName;
  private Date promoStart;
  private Date promoEnd;
  private int promoQuota;
  private Set<PromoMerchantRuleComboRequest> promoRuleComboSet;
  private Set<PromoMerchantRuleWholesaleRequest> promoRuleWholesaleSet;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public Date getPromoEnd() {
    return this.promoEnd;
  }

  public String getPromoId() {
    return this.promoId;
  }

  public String getPromoName() {
    return this.promoName;
  }

  public int getPromoQuota() {
    return this.promoQuota;
  }

  public Set<PromoMerchantRuleComboRequest> getPromoRuleComboSet() {
    return this.promoRuleComboSet;
  }

  public Set<PromoMerchantRuleWholesaleRequest> getPromoRuleWholesaleSet() {
    return this.promoRuleWholesaleSet;
  }

  public Date getPromoStart() {
    return this.promoStart;
  }

  public String getPromoType() {
    return this.promoType;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setPromoEnd(Date promoEnd) {
    this.promoEnd = promoEnd;
  }

  public void setPromoId(String promoId) {
    this.promoId = promoId;
  }

  public void setPromoName(String promoName) {
    this.promoName = promoName;
  }

  public void setPromoQuota(int promoQuota) {
    this.promoQuota = promoQuota;
  }

  public void setPromoRuleComboSet(Set<PromoMerchantRuleComboRequest> promoRuleComboSet) {
    this.promoRuleComboSet = promoRuleComboSet;
  }

  public void setPromoRuleWholesaleSet(
      Set<PromoMerchantRuleWholesaleRequest> promoRuleWholesaleSet) {
    this.promoRuleWholesaleSet = promoRuleWholesaleSet;
  }

  public void setPromoStart(Date promoStart) {
    this.promoStart = promoStart;
  }

  public void setPromoType(String promoType) {
    this.promoType = promoType;
  }

  @Override
  public String toString() {
    return String.format(
        "PromoMerchantUpdateRequest [itemSku=%s, promoId=%s, promoType=%s, promoName=%s, promoStart=%s, promoEnd=%s, promoQuota=%s, promoRuleComboSet=%s, promoRuleWholesaleSet=%s, toString()=%s]",
        this.itemSku, this.promoId, this.promoType, this.promoName, this.promoStart, this.promoEnd,
        this.promoQuota, this.promoRuleComboSet, this.promoRuleWholesaleSet, super.toString());
  }
}
