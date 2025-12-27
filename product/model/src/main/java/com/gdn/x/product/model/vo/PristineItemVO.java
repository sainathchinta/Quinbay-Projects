package com.gdn.x.product.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineItemVO implements Serializable{
  private static final long serialVersionUID = 8L;

  private String merchantCode;
  private String itemSku;
  private String productSku;
  private String itemCode;
  private boolean isSynchronized;
  private Set<Price> price;
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<ItemViewConfig>();
  private Boolean isLateFulfillment;
  private Boolean off2OnChannelActive;
  private Boolean isArchived;
  private boolean promoBundling;
  private boolean cncActivated;
  private String uniqueId;
  private List<OfflineItemDetailVo> offlineItems;

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
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

  public String getItemCode() {
    return itemCode;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public boolean isSynchronized() {
    return isSynchronized;
  }

  public void setSynchronized(boolean aSynchronized) {
    isSynchronized = aSynchronized;
  }

  public Set<Price> getPrice() {
    return price;
  }

  public void setPrice(Set<Price> price) {
    this.price = price;
  }

  public Set<ItemViewConfig> getItemViewConfigs() {
    return itemViewConfigs;
  }

  public void setItemViewConfigs(Set<ItemViewConfig> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  public Boolean getLateFulfillment() {
    return isLateFulfillment;
  }

  public void setLateFulfillment(Boolean lateFulfillment) {
    isLateFulfillment = lateFulfillment;
  }

  public Boolean getOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(Boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public Boolean getArchived() {
    return isArchived;
  }

  public void setArchived(Boolean archived) {
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

  public String getUniqueId() {
    return uniqueId;
  }

  public void setUniqueId(String uniqueId) {
    this.uniqueId = uniqueId;
  }

  public List<OfflineItemDetailVo> getOfflineItems() {
    return offlineItems;
  }

  public void setOfflineItems(List<OfflineItemDetailVo> offlineItems) {
    this.offlineItems = offlineItems;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("PristineItemVO{");
    sb.append("merchantCode='").append(merchantCode).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
    sb.append(", isSynchronized=").append(isSynchronized);
    sb.append(", price=").append(price);
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append(", isLateFulfillment=").append(isLateFulfillment);
    sb.append(", off2OnChannelActive=").append(off2OnChannelActive);
    sb.append(", isArchived=").append(isArchived);
    sb.append(", promoBundling=").append(promoBundling);
    sb.append(", cncActivated=").append(cncActivated);
    sb.append(", uniqueId='").append(uniqueId).append('\'');
    sb.append(", offlineItems=").append(offlineItems);
    sb.append('}');
    return sb.toString();
  }
}
