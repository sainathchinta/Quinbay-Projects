package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineItemChange extends ProductBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -4413566987758524381L;
  private String itemSku;
  private String productSku;
  private String merchantSku;
  private boolean isSynchronized;
  private PristineDataItemEventModel pristineDataItem;
  private MasterDataItem masterDataItem;
  private boolean off2OnChannelActive;
  private boolean cncActivated;
  private boolean isArchived;
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<ItemViewConfig>();

  public PristineItemChange() {
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
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

  public boolean isOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public boolean isSynchronized() {
    return isSynchronized;
  }

  public void setSynchronized(boolean aSynchronized) {
    isSynchronized = aSynchronized;
  }

  public MasterDataItem getMasterDataItem() {
    return masterDataItem;
  }

  public void setMasterDataItem(MasterDataItem masterDataItem) {
    this.masterDataItem = masterDataItem;
  }

  public boolean isArchived() {
    return isArchived;
  }

  public void setArchived(boolean archived) {
    isArchived = archived;
  }

  public Set<ItemViewConfig> getItemViewConfigs() {
    return itemViewConfigs;
  }

  public void setItemViewConfigs(Set<ItemViewConfig> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("PristineItemChange{");
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", isSynchronized='").append(isSynchronized).append('\'');
    sb.append(", off2OnChannelActive='").append(off2OnChannelActive).append('\'');
    sb.append(", pristineDataItem=").append(pristineDataItem);
    sb.append(", cncActivated=").append(cncActivated);
    sb.append(", isArchived=").append(isArchived);
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append('}');
    return sb.toString();
  }
}
