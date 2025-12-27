package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;

/**
 * Created by govind on 01/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleItemVO implements Serializable{

  private static final long serialVersionUID = -8257551654675299383L;
  private String itemSku;
  private String itemCode;
  private String productSku;
  private boolean isSynchronized;
  private Set<Price> prices;
  private Set<ItemViewConfig> itemViewConfigs;
  private boolean isArchived ;
  private boolean promoBundling;
  private boolean cncActivated;
  private String pickupPointCode;
  private Set<String> activePromoBundlings;
  private boolean off2OnChannelActive ;
  private Date createdDate;
  private List<OfflineItemDetailVo> offlineItems;
  private SimplePristineDataItemVO simplePristineDataItem;
  private SimpleAsyncMasterDataItemVO simpleAsyncMasterDataItem;
  private boolean isSubscribable;

  public SimpleItemVO() {
  }

  public SimpleItemVO(String itemSku, String productSku) {
    this.itemSku = itemSku;
    this.productSku = productSku;
  }

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

  public Set<Price> getPrices() {
    return prices;
  }

  public void setPrices(Set<Price> prices) {
    this.prices = prices;
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

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
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

  public Date getCreatedDate() {
    return createdDate;
  }

  public void setCreatedDate(Date createdDate) {
    this.createdDate = createdDate;
  }

  public List<OfflineItemDetailVo> getOfflineItems() {
    return offlineItems;
  }

  public void setOfflineItems(List<OfflineItemDetailVo> offlineItems) {
    this.offlineItems = offlineItems;
  }

  public SimplePristineDataItemVO getSimplePristineDataItem() {
    return simplePristineDataItem;
  }

  public void setSimplePristineDataItem(SimplePristineDataItemVO simplePristineDataItem) {
    this.simplePristineDataItem = simplePristineDataItem;
  }

  public SimpleAsyncMasterDataItemVO getSimpleAsyncMasterDataItem() {
    return simpleAsyncMasterDataItem;
  }

  public void setSimpleAsyncMasterDataItem(
      SimpleAsyncMasterDataItemVO simpleAsyncMasterDataItem) {
    this.simpleAsyncMasterDataItem = simpleAsyncMasterDataItem;
  }

  public boolean isSubscribable() {
    return isSubscribable;
  }

  public void setSubscribable(boolean subscribable) {
    isSubscribable = subscribable;
  }

  @JsonIgnore
  public static SimpleItemVO toSimpleItemVo(Item item, SimpleMasterDataItemVO masterDataItemVO){
    SimpleItemVO simpleItemVO = new SimpleItemVO();
    simpleItemVO.setItemSku(item.getItemSku());
    simpleItemVO.setItemCode(item.getItemCode());
    simpleItemVO.setSynchronized(item.isSynchronized());
    simpleItemVO.setArchived(item.isArchived());
    simpleItemVO.setCncActivated(item.isCncActivated());
    simpleItemVO.setPickupPointCode(item.getPickupPointCode());
    simpleItemVO.setProductSku(item.getProductSku());
    simpleItemVO.setPromoBundling(item.isPromoBundling());
    simpleItemVO.setOff2OnChannelActive(item.isOff2OnChannelActive());
    simpleItemVO.setPrices(item.getPrice());
    simpleItemVO.setItemViewConfigs(item.getItemViewConfigs());
    simpleItemVO.setOfflineItems(item.getOfflineItems());
    simpleItemVO.setActivePromoBundlings(item.getActivePromoBundlings());
    simpleItemVO.setCreatedDate(item.getCreatedDate());
    if (Objects.nonNull(item.getPristineDataItem())) {
      simpleItemVO.setSimplePristineDataItem(
          SimplePristineDataItemVO.toSimplePristineDataItemVO(item.getPristineDataItem()));
    }
    if (!item.isSynchronized() && Objects.isNull(item.getPristineDataItem())) {
      SimpleAsyncMasterDataItemVO simpleAsyncMasterDataItemVO =
          SimpleAsyncMasterDataItemVO.toSimpleAsyncMasterDataItemVO(item);
      if (Objects.nonNull(masterDataItemVO) && CollectionUtils.isNotEmpty(masterDataItemVO.getMasterDataItemImages())) {
        simpleAsyncMasterDataItemVO.setMasterDataItemImages(masterDataItemVO.getMasterDataItemImages());
      }
      simpleItemVO.setSimpleAsyncMasterDataItem(simpleAsyncMasterDataItemVO);
    }
    simpleItemVO.setSubscribable(item.isSubscribable());
    return simpleItemVO;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("itemSku", itemSku).append("itemCode", itemCode)
        .append("productSku", productSku).append("isSynchronized", isSynchronized)
        .append("prices", prices).append("itemViewConfigs", itemViewConfigs)
        .append("isArchived", isArchived).append("promoBundling", promoBundling)
        .append("cncActivated", cncActivated).append("pickupPointCode", pickupPointCode)
        .append("activePromoBundlings", activePromoBundlings)
        .append("off2OnChannelActive", off2OnChannelActive).append("createdDate", createdDate)
        .append("offlineItems", offlineItems)
        .append("simplePristineDataItem", simplePristineDataItem)
        .append("simpleAsyncMasterDataItem", simpleAsyncMasterDataItem)
        .append("isSubscribable", isSubscribable).toString();
  }
}
