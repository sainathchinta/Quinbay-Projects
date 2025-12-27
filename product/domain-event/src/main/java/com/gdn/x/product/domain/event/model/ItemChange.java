package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.domain.event.enums.ItemChangeEventType;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemChange extends ProductBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 1L;

  private String merchantCode;
  private String itemSku;
  private String productSku;
  private String merchantSku;
  private String itemCode;
  private boolean isSynchronized;
  private String itemCatentryId;
  private MasterDataItem masterDataItem;
  private Set<Price> price = new HashSet<Price>();
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<ItemViewConfig>();
  private Boolean isLateFulfillment;
  private String pickupPointCode;
  private boolean off2OnChannelActive;
  private boolean isArchived;
  private PristineDataItemEventModel pristineDataItem;
  private boolean cncActivated;
  private String uniqueId;
  private List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
  private boolean isSubscribable;
  private boolean forceReview;
  private boolean isContentChanged;
  private boolean wholesalePriceExists;
  private boolean merchantPromoDiscount;
  private boolean isFlashSaleActive;
  private boolean promoBundling;
  private Set<String> activePromoBundlings;

  public ItemChange() {

  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public Boolean getIsLateFulfillment() {
    return this.isLateFulfillment;
  }

  public String getItemCatentryId() {
    return this.itemCatentryId;
  }

  public String getItemCode() {
    return this.itemCode;
  }

  public String getItemSku() {
    return this.itemSku;
  }

  public Set<ItemViewConfig> getItemViewConfigs() {
    return this.itemViewConfigs;
  }

  public MasterDataItem getMasterDataItem() {
    return this.masterDataItem;
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

  public String getProductSku() {
    return this.productSku;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public Boolean isLateFulfillment() {
    return this.isLateFulfillment;
  }

  public boolean isOff2OnChannelActive() {
    return this.off2OnChannelActive;
  }

  public boolean isSynchronized() {
    return this.isSynchronized;
  }

  public void setIsLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setItemCatentryId(String itemCatentryId) {
    this.itemCatentryId = itemCatentryId;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public void setItemViewConfigs(Set<ItemViewConfig> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  public void setLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setMasterDataItem(MasterDataItem masterDataItem) {
    this.masterDataItem = masterDataItem;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setPrice(Set<Price> price) {
    this.price = price;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setSynchronized(boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

  public boolean isArchived() {
    return isArchived;
  }

  public void setArchived(boolean isArchived) {
    this.isArchived = isArchived;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public PristineDataItemEventModel getPristineDataItem() {
    return pristineDataItem;
  }

  public void setPristineDataItem(PristineDataItemEventModel pristineDataItem) {
    this.pristineDataItem = pristineDataItem;
  }

  public boolean isCncActivated() {
    return cncActivated;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  public String getUniqueId() {
    return uniqueId;
  }

  public void setUniqueId(String uniqueId) {
    this.uniqueId = uniqueId;
  }

  public List<ItemChangeEventType> getItemChangeEventTypes() {
    return itemChangeEventTypes;
  }

  public void setItemChangeEventTypes(List<ItemChangeEventType> itemChangeEventTypes) {
    this.itemChangeEventTypes = itemChangeEventTypes;
  }

  public boolean isSubscribable() {
    return isSubscribable;
  }

  public void setSubscribable(boolean subscribable) {
    isSubscribable = subscribable;
  }

  public boolean isForceReview() {
    return forceReview;
  }

  public void setForceReview(boolean forceReview) {
    this.forceReview = forceReview;
  }

  public boolean isContentChanged() {
    return isContentChanged;
  }

  public void setContentChanged(boolean contentChanged) {
    isContentChanged = contentChanged;
  }

  public boolean isWholesalePriceExists() {
    return wholesalePriceExists;
  }

  public void setWholesalePriceExists(boolean wholesalePriceExists) {
    this.wholesalePriceExists = wholesalePriceExists;
  }

  public boolean isMerchantPromoDiscount() {
    return merchantPromoDiscount;
  }

  public void setMerchantPromoDiscount(boolean merchantPromoDiscount) {
    this.merchantPromoDiscount = merchantPromoDiscount;
  }

  public boolean isFlashSaleActive() {
    return isFlashSaleActive;
  }

  public void setFlashSaleActive(boolean flashSaleActive) {
    isFlashSaleActive = flashSaleActive;
  }

  public boolean isPromoBundling() {
    return promoBundling;
  }

  public void setPromoBundling(boolean promoBundling) {
    this.promoBundling = promoBundling;
  }

  public Set<String> getActivePromoBundlings() {
    return activePromoBundlings;
  }

  public void setActivePromoBundlings(Set<String> activePromoBundlings) {
    this.activePromoBundlings = activePromoBundlings;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemChange{");
    sb.append("merchantCode='").append(merchantCode).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
    sb.append(", isSynchronized=").append(isSynchronized);
    sb.append(", itemCatentryId='").append(itemCatentryId).append('\'');
    sb.append(", masterDataItem=").append(masterDataItem);
    sb.append(", price=").append(price);
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append(", isLateFulfillment=").append(isLateFulfillment);
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", off2OnChannelActive=").append(off2OnChannelActive);
    sb.append(", isArchived=").append(isArchived);
    sb.append(", pristineDataItem=").append(pristineDataItem);
    sb.append(", cncActivated=").append(cncActivated);
    sb.append(", uniqueId='").append(uniqueId).append('\'');
    sb.append(", itemChangeEventTypes='").append(itemChangeEventTypes).append('\'');
    sb.append(", isSubscribable=").append(isSubscribable);
    sb.append(", forceReview=").append(forceReview);
    sb.append(", isContentChanged=").append(isContentChanged);
    sb.append(", wholesalePriceExists=").append(wholesalePriceExists);
    sb.append(", promoBundling=").append(promoBundling);
    sb.append(", activePromoBundlings=").append(activePromoBundlings);
    sb.append(", isFlashSaleActive=").append(isFlashSaleActive);
    sb.append(", merchantPromoDiscount=").append(merchantPromoDiscount);
    sb.append('}');
    return sb.toString();
  }

}
