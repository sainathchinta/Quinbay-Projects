package com.gdn.x.product.rest.web.model.dto;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;

import lombok.ToString;

@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemDTO extends BaseRequest {

  private static final long serialVersionUID = 1L;

  private String merchantCode;
  private String merchantSku;
  private String itemCode;
  private String itemSku;
  private Set<PriceRequest> price;
  private Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<ItemViewConfigRequest>();
  private Boolean isLateFulfillment;
  private String pickupPointCode;
  private boolean forceReview = false;
  private boolean wholesalePriceExists = false;
  private boolean wholesalePriceActivated = false;
  private boolean markForDelete;
  private boolean freeSample = false;

  public ItemDTO() {

  }

  public ItemDTO(String merchantSku, String itemCode, Set<PriceRequest> price,
      Set<ItemViewConfigRequest> itemViewConfigs, Boolean isLateFulfillment,
      String pickupPointCode) {
    super();
    this.merchantSku = merchantSku;
    this.itemCode = itemCode;
    this.price = price;
    this.itemViewConfigs = itemViewConfigs;
    this.isLateFulfillment = isLateFulfillment;
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public Boolean getIsLateFulfillment() {
    return this.isLateFulfillment;
  }

  public String getItemCode() {
    return this.itemCode;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public Set<ItemViewConfigRequest> getItemViewConfigs() {
    return this.itemViewConfigs;
  }

  public String getMerchantSku() {
    return this.merchantSku;
  }

  public String getPickupPointCode() {
    return this.pickupPointCode;
  }

  public Set<PriceRequest> getPrice() {
    return this.price;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Deprecated
  public Boolean isLateFulfillment() {
    return this.isLateFulfillment;
  }

  public void setIsLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setItemViewConfigs(Set<ItemViewConfigRequest> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  @Deprecated
  public void setLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setPrice(Set<PriceRequest> price) {
    this.price = price;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public boolean isForceReview() {
    return this.forceReview;
  }

  public void setForceReview(boolean forceReview) {
    this.forceReview = forceReview;
  }

  public boolean isWholesalePriceExists() {
    return wholesalePriceExists;
  }

  public void setWholesalePriceExists(boolean wholesalePriceExists) {
    this.wholesalePriceExists = wholesalePriceExists;
  }

  public boolean isWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public boolean isFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }
}
