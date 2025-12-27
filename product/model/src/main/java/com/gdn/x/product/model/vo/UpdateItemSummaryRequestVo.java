package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;

public class UpdateItemSummaryRequestVo implements Serializable {
  private static final long serialVersionUID = 1L;

  private String merchantSku;
  private String pickupPointCode;
  private Boolean isLateFulfillment;
  private Set<Price> price = new HashSet<>();
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
  private Boolean off2OnChannelActive;

  public UpdateItemSummaryRequestVo() {}


  public UpdateItemSummaryRequestVo(String merchantSku, String pickupPointCode,
      Boolean isLateFulfillment, Set<Price> price, Set<ItemViewConfig> itemViewConfigs) {
    super();
    this.merchantSku = merchantSku;
    this.pickupPointCode = pickupPointCode;
    this.isLateFulfillment = isLateFulfillment;
    this.price = price;
    this.itemViewConfigs = itemViewConfigs;
  }

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }

  public Boolean getIsLateFulfillment() {
    return this.isLateFulfillment;
  }

  public Set<ItemViewConfig> getItemViewConfigs() {
    return this.itemViewConfigs;
  }

  public String getMerchantSku() {
    return this.merchantSku;
  }

  public String getPickupPointCode() {
    return this.pickupPointCode;
  }

  public Set<Price> getPrice() {
    return this.price;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setIsLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setItemViewConfigs(Set<ItemViewConfig> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setPrice(Set<Price> price) {
    this.price = price;
  }

  public Boolean getOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(Boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  @Override
  public String toString() {
    return String.format(
        "UpdateItemSummaryRequestVo [merchantSku=%s, pickupPointCode=%s, isLateFulfillment=%s, price=%s, " +
            "itemViewConfigs=%s, off2OnChannelActive=%s toString()=%s]",
        this.merchantSku, this.pickupPointCode, this.isLateFulfillment, this.price,
        this.itemViewConfigs, this.off2OnChannelActive, super.toString());
  }

}
