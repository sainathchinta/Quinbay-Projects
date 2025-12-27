package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemViewConfigWithArchivedChangeEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 860513259229334206L;

  private String itemSku;
  private String pickupPointCode;
  private String merchantCode;
  private boolean isArchived;
  private String storeId;
  private boolean markForDelete;
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<>();

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public Set<ItemViewConfig> getItemViewConfigs() {
    return itemViewConfigs;
  }

  public void setItemViewConfigs(Set<ItemViewConfig> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  public boolean isArchived() {
    return isArchived;
  }

  public void setIsArchived(boolean isArchived) {
    this.isArchived = isArchived;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  @Override
  public String toString() {
    final StringBuffer sb = new StringBuffer("ItemViewConfigWithArchivedChangeEvent{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", isArchived=").append(isArchived);
    sb.append(", storeId='").append(storeId).append('\'');
    sb.append(", markForDelete=").append(markForDelete);
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append(", pickupPointCode=").append(pickupPointCode);
    sb.append('}');
    return sb.toString();
  }
}
