package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.WholesaleRule;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemInfo;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;

import java.util.List;
import java.util.Set;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemInfoResponse extends BaseResponse {

  private static final long serialVersionUID = -4551155664082522806L;

  private String merchantCode;

  private String itemSku;

  private String productSku;

  private String merchantSku;

  private String itemCode;

  private boolean isSynchronized;

  private String itemCatentryId;

  private MasterDataItemInfo masterDataItem;

  private PristineDataItemDto pristineDataItem;

  private Set<PriceDTO> price;

  private PriceDTO channelPrice;

  private Set<ItemViewConfigDTO> itemViewConfigs;

  private ItemViewConfigDTO channelItemViewConfig;

  private Boolean isLateFulfillment;

  private String pickupPointCode;

  private String ticketTemplateCode;

  private String etdNote;

  private Set<String> activePromoBundlings;

  private List<WholesaleRule> wholesaleRules;

  private boolean cncActivated;

  private String uniqueId;

  private List<OfflineItemDetailResponse> offlineItems;

  private boolean isSubscribable;

  private Set<String> sellerActivePromoBundlings;

  public ItemInfoResponse() {}

  @Override
  public boolean equals(Object anObject) {
    return GdnObjects.equals(this, anObject);
  }

  public ItemViewConfigDTO getChannelItemViewConfig() {
    return channelItemViewConfig;
  }

  public PriceDTO getChannelPrice() {
    return channelPrice;
  }

  public boolean isCncActivated() {
    return cncActivated;
  }

  public String getEtdNote() {
    return etdNote;
  }

  public Boolean getIsLateFulfillment() {
    return isLateFulfillment;
  }

  public String getItemCatentryId() {
    return itemCatentryId;
  }

  public String getItemCode() {
    return itemCode;
  }

  public String getItemSku() {
    return itemSku;
  }

  public Set<ItemViewConfigDTO> getItemViewConfigs() {
    return itemViewConfigs;
  }

  public MasterDataItemInfo getMasterDataItem() {
    return masterDataItem;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public List<OfflineItemDetailResponse> getOfflineItems() {
    return offlineItems;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public Set<PriceDTO> getPrice() {
    return price;
  }

  public PristineDataItemDto getPristineDataItem() {
    return pristineDataItem;
  }

  public String getProductSku() {
    return productSku;
  }

  public String getTicketTemplateCode() {
    return ticketTemplateCode;
  }

  public String getUniqueId() {
    return uniqueId;
  }

  public Set<String> getActivePromoBundlings() {
    return activePromoBundlings;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isSynchronized() {
    return isSynchronized;
  }

  public void setChannelItemViewConfig(ItemViewConfigDTO channelItemViewConfig) {
    this.channelItemViewConfig = channelItemViewConfig;
  }

  public void setChannelPrice(PriceDTO channelPrice) {
    this.channelPrice = channelPrice;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  public void setEtdNote(String etdNote) {
    this.etdNote = etdNote;
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

  public void setItemViewConfigs(Set<ItemViewConfigDTO> itemViewConfigs) {
    this.itemViewConfigs = itemViewConfigs;
  }

  public void setMasterDataItem(MasterDataItemInfo masterDataItem) {
    this.masterDataItem = masterDataItem;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setOfflineItems(List<OfflineItemDetailResponse> offlineItems) {
    this.offlineItems = offlineItems;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setPrice(Set<PriceDTO> price) {
    this.price = price;
  }

  public void setPristineDataItem(PristineDataItemDto pristineDataItem) {
    this.pristineDataItem = pristineDataItem;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setSynchronized(boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

  public void setTicketTemplateCode(String ticketTemplateCode) {
    this.ticketTemplateCode = ticketTemplateCode;
  }

  public void setActivePromoBundlings(Set<String> activePromoBundlings) {
    this.activePromoBundlings = activePromoBundlings;
  }

  public List<WholesaleRule> getWholesaleRules() {
    return wholesaleRules;
  }

  public void setWholesaleRules(List<WholesaleRule> wholesaleRules) {
    this.wholesaleRules = wholesaleRules;
  }

  public void setUniqueId(String uniqueId) {
    this.uniqueId = uniqueId;
  }

  public boolean isSubscribable() {
    return isSubscribable;
  }

  public void setSubscribable(boolean subscribable) {
    isSubscribable = subscribable;
  }

  public Set<String> getSellerActivePromoBundlings() {
    return sellerActivePromoBundlings;
  }

  public void setSellerActivePromoBundlings(Set<String> sellerActivePromoBundlings) {
    this.sellerActivePromoBundlings = sellerActivePromoBundlings;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemInfoResponse{");
    sb.append("merchantCode='").append(merchantCode).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
    sb.append(", isSynchronized=").append(isSynchronized);
    sb.append(", itemCatentryId='").append(itemCatentryId).append('\'');
    sb.append(", masterDataItem=").append(masterDataItem);
    sb.append(", pristineDataItem=").append(pristineDataItem);
    sb.append(", price=").append(price);
    sb.append(", channelPrice=").append(channelPrice);
    sb.append(", itemViewConfigs=").append(itemViewConfigs);
    sb.append(", channelItemViewConfig=").append(channelItemViewConfig);
    sb.append(", isLateFulfillment=").append(isLateFulfillment);
    sb.append(", pickupPointCode='").append(pickupPointCode).append('\'');
    sb.append(", ticketTemplateCode='").append(ticketTemplateCode).append('\'');
    sb.append(", etdNote='").append(etdNote).append('\'');
    sb.append(", activePromoBundlings=").append(activePromoBundlings);
    sb.append(", wholesaleRules=").append(wholesaleRules);
    sb.append(", cncActivated=").append(cncActivated);
    sb.append(", uniqueId='").append(uniqueId).append('\'');
    sb.append(", offlineItems=").append(offlineItems);
    sb.append(", isSubscribable=").append(isSubscribable);
    sb.append(", sellerActivePromoBundlings=").append(sellerActivePromoBundlings);
    sb.append('}');
    return sb.toString();
  }
}
