package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.PristineMasterDataItemDTO;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineItemResponse implements Serializable {

  private static final long serialVersionUID = 1L;
  private String itemSku;
  private String productSku;
  private String merchantCode;
  private String itemCode;
  private boolean isSynchronized;
  private String pristineProductName;
  private Set<PriceDTO> price = new HashSet<PriceDTO>();
  private Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<ItemViewConfigDTO>();
  private Boolean isLateFulfillment = false;
  private boolean off2OnChannelActive;
  private boolean isArchived = false;
  private boolean promoBundling = false;
  private PristineMasterDataItemDTO masterDataItem;
  private Set<String> activePromoBundlings;
  private boolean cncActivated;
  private String uniqueId;
  private List<OfflineItemDetailResponse> offlineItems;

  public PristineItemResponse() {
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

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
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

  public String getPristineProductName() {
    return pristineProductName;
  }

  public void setPristineProductName(String pristineProductName) {
    this.pristineProductName = pristineProductName;
  }

  public Set<PriceDTO> getPrice() {
    return price;
  }

  public void setPrice(Set<PriceDTO> price) {
    this.price = price;
  }

  public Set<ItemViewConfigDTO> getItemViewConfigs() {
    return itemViewConfigs;
  }

  public void setItemViewConfigs(Set<ItemViewConfigDTO> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  public Boolean getLateFulfillment() {
    return isLateFulfillment;
  }

  public void setLateFulfillment(Boolean lateFulfillment) {
    isLateFulfillment = lateFulfillment;
  }

  public boolean isOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
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

  public PristineMasterDataItemDTO getMasterDataItem() {
    return masterDataItem;
  }

  public void setMasterDataItem(PristineMasterDataItemDTO masterDataItem) {
    this.masterDataItem = masterDataItem;
  }

  public Set<String> getActivePromoBundlings() {
    return activePromoBundlings;
  }

  public void setActivePromoBundlings(Set<String> activePromoBundlings) {
    this.activePromoBundlings = activePromoBundlings;
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

  public List<OfflineItemDetailResponse> getOfflineItems() {
    return offlineItems;
  }

  public void setOfflineItems(List<OfflineItemDetailResponse> offlineItems) {
    this.offlineItems = offlineItems;
  }

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("PristineItemResponse{");
    sb.append("itemSku='").append(itemSku).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", merchantCode='").append(merchantCode).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
    sb.append(", isSynchronized=").append(isSynchronized);
    sb.append(", pristineProductName='").append(pristineProductName).append('\'');
    sb.append(", price=").append(price);
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append(", isLateFulfillment=").append(isLateFulfillment);
    sb.append(", off2OnChannelActive=").append(off2OnChannelActive);
    sb.append(", isArchived=").append(isArchived);
    sb.append(", promoBundling=").append(promoBundling);
    sb.append(", masterDataItem=").append(masterDataItem);
    sb.append(", activePromoBundlings=").append(activePromoBundlings);
    sb.append(", cncActivated=").append(cncActivated);
    sb.append(", uniqueId='").append(uniqueId).append('\'');
    sb.append(", offlineItems=").append(offlineItems);
    sb.append('}');
    return sb.toString();
  }
}

