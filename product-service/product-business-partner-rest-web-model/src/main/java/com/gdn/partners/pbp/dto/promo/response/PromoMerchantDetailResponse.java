package com.gdn.partners.pbp.dto.promo.response;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoMerchantDetailResponse extends BaseResponse {
  private static final long serialVersionUID = 285566480458491191L;

  private String businessPartnerCode;
  private String itemSku;
  private String promoId;
  private String promoType;
  private String promoName;
  private String promoStatus;
  private Date promoStart;
  private Date promoEnd;
  private int promoQuota;
  private int promoSold;
  private List<PromoMerchantRuleComboResponse> promoRuleComboSet;
  private List<PromoMerchantRuleWholesaleResponse> promoRuleWholesaleSet;

  // Additional Information
  private PromoMerchantInfoItemResponse infoItem;
  private PromoMerchantInfoInventoryResponse infoInventory;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBusinessPartnerCode() {
    return this.businessPartnerCode;
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

  public List<PromoMerchantRuleComboResponse> getPromoRuleComboSet() {
    return this.promoRuleComboSet;
  }

  public List<PromoMerchantRuleWholesaleResponse> getPromoRuleWholesaleSet() {
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

  public void setInfoInventory(PromoMerchantInfoInventoryResponse infoInventory) {
    this.infoInventory = infoInventory;
  }

  public void setInfoItem(PromoMerchantInfoItemResponse infoItem) {
    this.infoItem = infoItem;
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

  public void setPromoRuleComboSet(List<PromoMerchantRuleComboResponse> promoRuleComboSet) {
    this.promoRuleComboSet = promoRuleComboSet;
  }

  public void setPromoRuleWholesaleSet(
      List<PromoMerchantRuleWholesaleResponse> promoRuleWholesaleSet) {
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
        "PromoMerchantDetailResponse [businessPartnerCode=%s, itemSku=%s, promoId=%s, promoType=%s, promoName=%s, promoStatus=%s, promoStart=%s, promoEnd=%s, promoQuota=%s, promoSold=%s, promoRuleComboSet=%s, promoRuleWholesaleSet=%s, infoItem=%s, infoInventory=%s, toString()=%s]",
        this.businessPartnerCode, this.itemSku, this.promoId, this.promoType, this.promoName,
        this.promoStatus, this.promoStart, this.promoEnd, this.promoQuota, this.promoSold,
        this.promoRuleComboSet, this.promoRuleWholesaleSet, this.infoItem, this.infoInventory,
        super.toString());
  }
}
