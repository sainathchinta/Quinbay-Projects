package com.gdn.x.product.rest.web.model.request;

import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private String itemSku;
  private String productSku;
  private String merchantSku;
  private String itemCode;
  private double shippingWeight;
  private MasterDataItemDTO masterDataItem;
  private Set<PriceRequest> price = new HashSet<PriceRequest>();
  private Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<ItemViewConfigRequest>();
  private Boolean isLateFulfillment;
  private String pickupPointCode;
  private Boolean off2OnChannelActive;
  private Boolean wholesalePriceActivated;
  private boolean isContentChanged;
  private boolean freeSample;

  public ItemRequest() {

  }

  public ItemRequest(String itemSku, String productSku, String merchantSku, String itemCode,
      double shippingWeight, MasterDataItemDTO masterDataItem, Set<PriceRequest> price,
      Set<ItemViewConfigRequest> itemViewConfigs, Boolean isLateFulfillment, String pickupPointCode) {
    super();
    this.itemSku = itemSku;
    this.productSku = productSku;
    this.merchantSku = merchantSku;
    this.itemCode = itemCode;
    this.shippingWeight = shippingWeight;
    this.masterDataItem = masterDataItem;
    this.price = price;
    this.itemViewConfigs = itemViewConfigs;
    this.isLateFulfillment = isLateFulfillment;
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
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

  public MasterDataItemDTO getMasterDataItem() {
    return this.masterDataItem;
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

  public String getProductSku() {
    return this.productSku;
  }

  public double getShippingWeight() {
    return this.shippingWeight;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public Boolean isLateFulfillment() {
    return this.isLateFulfillment;
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

  public void setLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setMasterDataItem(MasterDataItemDTO masterDataItem) {
    this.masterDataItem = masterDataItem;
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

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setShippingWeight(double shippingWeight) {
    this.shippingWeight = shippingWeight;
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

  public boolean isContentChanged() {
    return isContentChanged;
  }

  public void setContentChanged(boolean contentChanged) {
    isContentChanged = contentChanged;
  }

  public boolean getFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ItemRequest [itemSku=%s, productSku=%s, merchantSku=%s, itemCode=%s, shippingWeight=%s, " +
                "masterDataItem=%s, price=%s, itemViewConfigs=%s, isLateFulfillment=%s, " +
                "pickupPointCode=%s, off2OnChannelActive=%s, isContentChanged=%s, toString()=%s]",
            this.itemSku, this.productSku, this.merchantSku, this.itemCode, this.shippingWeight,
            this.masterDataItem, this.price, this.itemViewConfigs, this.isLateFulfillment,
            this.pickupPointCode, off2OnChannelActive, isContentChanged, freeSample,
          super.toString());
  }

}
