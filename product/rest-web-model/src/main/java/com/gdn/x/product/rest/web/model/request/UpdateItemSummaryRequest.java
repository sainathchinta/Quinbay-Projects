package com.gdn.x.product.rest.web.model.request;

import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateItemSummaryRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private String merchantSku;
  private String pickupPointCode;
  private Boolean isLateFulfillment;
  private Set<PriceDTO> price;
  private Set<ItemViewConfigDTO> itemViewConfigs;
  private Boolean off2OnChannelActive;
  private Boolean wholesalePriceActivated;

  public UpdateItemSummaryRequest() {}


  public UpdateItemSummaryRequest(String merchantSku, String pickupPointCode,
      Boolean isLateFulfillment, Set<PriceDTO> price, Set<ItemViewConfigDTO> itemViewConfigs) {
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

  public Set<ItemViewConfigDTO> getItemViewConfigs() {
    return this.itemViewConfigs;
  }

  public String getMerchantSku() {
    return this.merchantSku;
  }

  public String getPickupPointCode() {
    return this.pickupPointCode;
  }

  public Set<PriceDTO> getPrice() {
    return this.price;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }


  public void setIsLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }


  public void setItemViewConfigs(Set<ItemViewConfigDTO> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }


  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }


  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }


  public void setPrice(Set<PriceDTO> price) {
    this.price = price;
  }

  public Boolean getOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(Boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public Boolean getWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(Boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  @Override
  public String toString() {
    return String.format(
        "UpdateItemSummaryRequest [merchantSku=%s, pickupPointCode=%s, isLateFulfillment=%s, price=%s, " +
            "itemViewConfigs=%s, off2OnChannelActive=%s, wholesalePriceActivated=%s, toString()=%s]",
        this.merchantSku, this.pickupPointCode, this.isLateFulfillment, this.price,
        this.itemViewConfigs, this.off2OnChannelActive, this.wholesalePriceActivated, super.toString());
  }
}
