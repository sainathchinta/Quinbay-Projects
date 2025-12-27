package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.Set;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.rest.web.model.response.OfflineItemDetailResponse;

/**
 * Created by govind on 16/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleItemResponseDTO implements Serializable{

  private static final long serialVersionUID = 6948787865412105667L;
  private String itemSku;
  private String itemCode;
  private String productSku;
  private boolean isSynchronized;
  private Set<PriceDTO> prices;
  private Set<ItemViewConfigDTO> itemViewConfigs;
  private boolean isArchived ;
  private boolean promoBundling;
  private boolean cncActivated;
  private String pickupPointCode;
  private Date createdDate;
  private Set<String> activePromoBundlings;
  private boolean off2OnChannelActive ;
  private List<OfflineItemDetailResponse> offlineItems;
  private SimplePristineDataItemDTO simplePristineDataItem;
  private SimpleAsyncMasterDataItemDTO simpleAsyncMasterDataItem;
  private boolean isSubscribable;

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public String getItemCode() {
    return itemCode;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public boolean isSynchronized() {
    return isSynchronized;
  }

  public void setSynchronized(boolean aSynchronized) {
    isSynchronized = aSynchronized;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Set<PriceDTO> getPrices() {
    return prices;
  }

  public void setPrices(Set<PriceDTO> prices) {
    this.prices = prices;
  }

  public Set<ItemViewConfigDTO> getItemViewConfigs() {
    return itemViewConfigs;
  }

  public void setItemViewConfigs(Set<ItemViewConfigDTO> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  public boolean isArchived() {
    return isArchived;
  }

  public void setArchived(boolean archived) {
    isArchived = archived;
  }

  public boolean isPromoBundling() {
    return promoBundling;
  }

  public void setPromoBundling(boolean promoBundling) {
    this.promoBundling = promoBundling;
  }

  public boolean isCncActivated() {
    return cncActivated;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  public Date getCreatedDate() {
    return createdDate;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public Set<String> getActivePromoBundlings() {
    return activePromoBundlings;
  }

  public void setActivePromoBundlings(Set<String> activePromoBundlings) {
    this.activePromoBundlings = activePromoBundlings;
  }

  public boolean isOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public List<OfflineItemDetailResponse> getOfflineItems() {
    return offlineItems;
  }

  public void setOfflineItems(List<OfflineItemDetailResponse> offlineItems) {
    this.offlineItems = offlineItems;
  }

  public SimplePristineDataItemDTO getSimplePristineDataItem() {
    return simplePristineDataItem;
  }

  public void setSimplePristineDataItem(SimplePristineDataItemDTO simplePristineDataItem) {
    this.simplePristineDataItem = simplePristineDataItem;
  }

  public SimpleAsyncMasterDataItemDTO getSimpleAsyncMasterDataItem() {
    return simpleAsyncMasterDataItem;
  }

  public void setSimpleAsyncMasterDataItem(SimpleAsyncMasterDataItemDTO simpleAsyncMasterDataItem) {
    this.simpleAsyncMasterDataItem = simpleAsyncMasterDataItem;
  }

  public boolean isSubscribable() {
    return isSubscribable;
  }

  public void setSubscribable(boolean subscribable) {
    isSubscribable = subscribable;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("itemSku", itemSku).append("itemCode", itemCode)
        .append("productSku", productSku).append("isSynchronized", isSynchronized)
        .append("prices", prices).append("itemViewConfigs", itemViewConfigs)
        .append("isArchived", isArchived).append("promoBundling", promoBundling)
        .append("cncActivated", cncActivated).append("createdDate", createdDate)
        .append("pickupPointCode", pickupPointCode)
        .append("activePromoBundlings", activePromoBundlings)
        .append("off2OnChannelActive", off2OnChannelActive).append("offlineItems", offlineItems)
        .append("simplePristineDataItem", simplePristineDataItem)
        .append("simpleAsyncMasterDataItem", simpleAsyncMasterDataItem)
        .append("isSubscribable", isSubscribable).toString();
  }
}
