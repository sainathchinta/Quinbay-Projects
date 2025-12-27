package com.gdn.partners.pbp.model.promo;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.gdn.common.base.GdnObjects;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PromoMerchant implements Serializable {
  private static final long serialVersionUID = -7588110338120192686L;

  private String businessPartnerCode;
  private String itemCode;
  private String itemSku;
  private String promoId;
  private String promoType;
  private String promoName;
  private String promoStatus;
  private Date promoStart;
  private Date promoEnd;
  private int promoQuota;
  private int promoSold;
  private Date createdDate;
  private List<PromoMerchantRuleCombo> promoRuleComboSet;
  private List<PromoMerchantRuleWholesale> promoRuleWholesaleSet;

  // Additional Information
  private PromoMerchantInfoItem infoItem;
  private PromoMerchantInfoInventory infoInventory;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBusinessPartnerCode() {
    return this.businessPartnerCode;
  }

  public Date getCreatedDate() {
    return this.createdDate;
  }

  public PromoMerchantInfoInventory getInfoInventory() {
    return this.infoInventory;
  }

  public PromoMerchantInfoItem getInfoItem() {
    return this.infoItem;
  }

  public String getItemCode() {
    return itemCode;
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

  public List<PromoMerchantRuleCombo> getPromoRuleComboSet() {
    return this.promoRuleComboSet;
  }

  public List<PromoMerchantRuleWholesale> getPromoRuleWholesaleSet() {
    return this.promoRuleWholesaleSet;
  }

  public int getPromoSold() {
    return this.promoSold;
  }

  public Date getPromoStart() {
    return this.promoStart;
  }

  public String getPromoStatus() {
    return this.promoStatus;
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

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public void setInfoInventory(PromoMerchantInfoInventory infoInventory) {
    this.infoInventory = infoInventory;
  }

  public void setInfoItem(PromoMerchantInfoItem infoItem) {
    this.infoItem = infoItem;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
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

  public void setPromoRuleComboSet(List<PromoMerchantRuleCombo> promoRuleComboSet) {
    this.promoRuleComboSet = promoRuleComboSet;
  }

  public void setPromoRuleWholesaleSet(List<PromoMerchantRuleWholesale> promoRuleWholesaleSet) {
    this.promoRuleWholesaleSet = promoRuleWholesaleSet;
  }

  public void setPromoSold(int promoSold) {
    this.promoSold = promoSold;
  }

  public void setPromoStart(Date promoStart) {
    this.promoStart = promoStart;
  }

  public void setPromoStatus(String promoStatus) {
    this.promoStatus = promoStatus;
  }

  public void setPromoType(String promoType) {
    this.promoType = promoType;
  }

  @Override
  public String toString() {
    return String.format(
        "PromoMerchant [businessPartnerCode=%s, itemCode=%s, itemSku=%s, promoId=%s, promoType=%s, promoName=%s, promoStatus=%s, promoStart=%s, promoEnd=%s, promoQuota=%s, promoSold=%s, createdDate=%s, promoRuleComboSet=%s, promoRuleWholesaleSet=%s, infoItem=%s, infoInventory=%s, toString()=%s]",
        this.businessPartnerCode, this.itemCode, this.itemSku, this.promoId, this.promoType, this.promoName,
        this.promoStatus, this.promoStart, this.promoEnd, this.promoQuota, this.promoSold,
        this.createdDate, this.promoRuleComboSet, this.promoRuleWholesaleSet, this.infoItem,
        this.infoInventory, super.toString());
  }
}
