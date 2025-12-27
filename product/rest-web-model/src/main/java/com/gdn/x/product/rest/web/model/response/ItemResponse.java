package com.gdn.x.product.rest.web.model.response;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;
import lombok.ToString;

@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemResponse extends BaseResponse {
  private static final long serialVersionUID = 1L;

  private String merchantCode;
  private String itemSku;
  private String productSku;
  private String merchantSku;
  private String itemCode;
  private boolean isSynchronized;
  private String itemCatentryId;
  private MasterDataItemDTO masterDataItem;
  private PristineDataItemDto pristineDataItem;
  private Set<PriceDTO> price;
  private Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<ItemViewConfigDTO>();
  private Boolean isLateFulfillment;
  private String pickupPointCode;
  private String ticketTemplateCode;
  private String etdNote;
  private Boolean off2OnChannelActive;
  private Boolean isArchived;
  private boolean promoBundling;
  private boolean cncActivated;
  private boolean cncActive;
  private Set<String> activePromoBundlings;
  private String uniqueId;
  private List<OfflineItemDetailResponse> offlineItems;
  private boolean merchantPromoDiscount;
  private boolean merchantPromoDiscountActive;
  private Boolean isArchivedBeforeSuspension;
  private boolean disableUnSync;
  private boolean priceEditDisabled;
  private boolean isSubscribable;
  private boolean subscribableAtL5Level;
  private boolean wholesalePriceExists;
  private Boolean wholesalePriceActivated;
  private String sourceItemCode;
  private boolean isContentChanged;
  private boolean markForDelete;
  private Long version;
  private boolean freeSample;
  private Set<String> sellerActivePromoBundlings;
  private boolean itemPickUpPointMarkForDelete;
  private String masterSku;

  public ItemResponse() {

  }

  public ItemResponse(String itemSku, String productSku, String merchantSku, String itemCode,
      boolean isSynchronized, String itemCatentryId, MasterDataItemDTO masterDataItem,
      Set<PriceDTO> price, Set<ItemViewConfigDTO> itemViewConfigs, Boolean isLateFulfillment,
      String pickupPointCode, String ticketTemplateCode, String etdNote) {
    super();
    this.itemSku = itemSku;
    this.productSku = productSku;
    this.merchantSku = merchantSku;
    this.itemCode = itemCode;
    this.isSynchronized = isSynchronized;
    this.itemCatentryId = itemCatentryId;
    this.masterDataItem = masterDataItem;
    this.price = price;
    this.itemViewConfigs = itemViewConfigs;
    this.isLateFulfillment = isLateFulfillment;
    this.pickupPointCode = pickupPointCode;
    this.ticketTemplateCode = ticketTemplateCode;
  }

  public ItemResponse(String merchantCode, String itemSku, String productSku, String merchantSku,
      String itemCode, boolean isSynchronized, String itemCatentryId,
      MasterDataItemDTO masterDataItem, PristineDataItemDto pristineDataItem, Set<PriceDTO> price,
      Set<ItemViewConfigDTO> itemViewConfigs, Boolean isLateFulfillment, String pickupPointCode,
      String ticketTemplateCode, String etdNote, Boolean off2OnChannelActive, Boolean isArchived,
      boolean promoBundling, boolean cncActivated, Set<String> activePromoBundlings,
      String uniqueId, List<OfflineItemDetailResponse> offlineItems) {
    this.merchantCode = merchantCode;
    this.itemSku = itemSku;
    this.productSku = productSku;
    this.merchantSku = merchantSku;
    this.itemCode = itemCode;
    this.isSynchronized = isSynchronized;
    this.itemCatentryId = itemCatentryId;
    this.masterDataItem = masterDataItem;
    this.pristineDataItem = pristineDataItem;
    this.price = price;
    this.itemViewConfigs = itemViewConfigs;
    this.isLateFulfillment = isLateFulfillment;
    this.pickupPointCode = pickupPointCode;
    this.ticketTemplateCode = ticketTemplateCode;
    this.etdNote = etdNote;
    this.off2OnChannelActive = off2OnChannelActive;
    this.isArchived = isArchived;
    this.promoBundling = promoBundling;
    this.cncActivated = cncActivated;
    this.activePromoBundlings = activePromoBundlings;
    this.uniqueId = uniqueId;
    this.offlineItems = offlineItems;
  }

  public ItemResponse(String merchantCode, String itemSku, String productSku, String merchantSku,
    String itemCode, boolean isSynchronized, String itemCatentryId,
    MasterDataItemDTO masterDataItem, PristineDataItemDto pristineDataItem, Set<PriceDTO> price,
    Set<ItemViewConfigDTO> itemViewConfigs, Boolean isLateFulfillment, String pickupPointCode,
    String ticketTemplateCode, String etdNote, Boolean off2OnChannelActive, Boolean isArchived,
    boolean promoBundling, boolean cncActivated, Set<String> activePromoBundlings,
    String uniqueId, List<OfflineItemDetailResponse> offlineItems, boolean freeSample) {
    this.merchantCode = merchantCode;
    this.itemSku = itemSku;
    this.productSku = productSku;
    this.merchantSku = merchantSku;
    this.itemCode = itemCode;
    this.isSynchronized = isSynchronized;
    this.itemCatentryId = itemCatentryId;
    this.masterDataItem = masterDataItem;
    this.pristineDataItem = pristineDataItem;
    this.price = price;
    this.itemViewConfigs = itemViewConfigs;
    this.isLateFulfillment = isLateFulfillment;
    this.pickupPointCode = pickupPointCode;
    this.ticketTemplateCode = ticketTemplateCode;
    this.etdNote = etdNote;
    this.off2OnChannelActive = off2OnChannelActive;
    this.isArchived = isArchived;
    this.promoBundling = promoBundling;
    this.cncActivated = cncActivated;
    this.activePromoBundlings = activePromoBundlings;
    this.uniqueId = uniqueId;
    this.offlineItems = offlineItems;
    this.freeSample = freeSample;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getEtdNote() {
    return this.etdNote;
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

  public Set<ItemViewConfigDTO> getItemViewConfigs() {
    return this.itemViewConfigs;
  }

  public MasterDataItemDTO getMasterDataItem() {
    return this.masterDataItem;
  }

  public String getMerchantSku() {
    return this.merchantSku;
  }

  public Boolean getOff2OnChannelActive() {
    return this.off2OnChannelActive;
  }

  public List<OfflineItemDetailResponse> getOfflineItems() {
    return offlineItems;
  }

  public String getPickupPointCode() {
    return this.pickupPointCode;
  }

  public Set<PriceDTO> getPrice() {
    return this.price;
  }

  public String getProductSku() {
    return this.productSku;
  }

  public String getTicketTemplateCode() {
    return this.ticketTemplateCode;
  }

  public String getUniqueId() {
    return uniqueId;
  }

  public Boolean getArchived() { return isArchived; }

  public Set<String> getActivePromoBundlings() {
    return activePromoBundlings;
  }

  public boolean getFreeSample() {
    return this.freeSample;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public Boolean isLateFulfillment() {
    return this.isLateFulfillment;
  }

  public boolean isSynchronized() {
    return this.isSynchronized;
  }

  public boolean getItemPickUpPointMarkForDelete() {return this.itemPickUpPointMarkForDelete;}

  public Boolean getArchivedBeforeSuspension() { return this.isArchivedBeforeSuspension; }

  public void setArchivedBeforeSuspension(Boolean isArchivedBeforeSuspension) { this.isArchivedBeforeSuspension = isArchivedBeforeSuspension; }

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

  public void setLateFulfillment(Boolean isLateFulfillment) {
    this.isLateFulfillment = isLateFulfillment;
  }

  public void setMasterDataItem(MasterDataItemDTO masterDataItem) {
    this.masterDataItem = masterDataItem;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setOff2OnChannelActive(Boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
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

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public void setSynchronized(boolean isSynchronized) {
    this.isSynchronized = isSynchronized;
  }

  public void setItemPickUpPointMarkForDelete(boolean itemPickUpPointMarkForDelete) {
    this.itemPickUpPointMarkForDelete = itemPickUpPointMarkForDelete;
  }

  public void setTicketTemplateCode(String ticketTemplateCode) {
    this.ticketTemplateCode = ticketTemplateCode;
  }

  public void setUniqueId(String uniqueId) {
    this.uniqueId = uniqueId;
  }

  public void setArchived(Boolean archived) { this.isArchived = archived; }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public PristineDataItemDto getPristineDataItem() {
    return pristineDataItem;
  }

  public void setPristineDataItem(PristineDataItemDto pristineDataItem) {
    this.pristineDataItem = pristineDataItem;
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

  public void setActivePromoBundlings(Set<String> activePromoBundlings) {
    this.activePromoBundlings = activePromoBundlings;
  }

  public Boolean getLateFulfillment() {
    return isLateFulfillment;
  }


  public boolean isMerchantPromoDiscount() {
    return merchantPromoDiscount;
  }

  public void setMerchantPromoDiscount(boolean merchantPromoDiscount) {
    this.merchantPromoDiscount = merchantPromoDiscount;
  }

  public boolean isMerchantPromoDiscountActive() {
    return merchantPromoDiscountActive;
  }

  public void setMerchantPromoDiscountActive(boolean merchantPromoDiscountActive) {
    this.merchantPromoDiscountActive = merchantPromoDiscountActive;
  }

  public boolean isPriceEditDisabled() {
    return priceEditDisabled;
  }

  public void setPriceEditDisabled(boolean priceEditDisabled) {
    this.priceEditDisabled = priceEditDisabled;
  }

  public boolean isDisableUnSync() {
    return disableUnSync;
  }

  public void setDisableUnSync(boolean disableUnSync) {
    this.disableUnSync = disableUnSync;
  }

  public boolean isSubscribable() {
    return isSubscribable;
  }

  public void setSubscribable(boolean subscribable) {
    isSubscribable = subscribable;
  }

  public boolean isWholesalePriceExists() {
    return wholesalePriceExists;
  }

  public void setWholesalePriceExists(boolean wholesalePriceExists) {
    this.wholesalePriceExists = wholesalePriceExists;
  }

  public Boolean getWholesalePriceActivated() {
    return wholesalePriceActivated;
  }

  public void setWholesalePriceActivated(Boolean wholesalePriceActivated) {
    this.wholesalePriceActivated = wholesalePriceActivated;
  }

  public String getSourceItemCode() {
    return sourceItemCode;
  }

  public void setSourceItemCode(String sourceItemCode) {
    this.sourceItemCode = sourceItemCode;
  }

  public boolean isContentChanged() {
    return isContentChanged;
  }

  public void setContentChanged(boolean contentChanged) {
    isContentChanged = contentChanged;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  public Set<String> getSellerActivePromoBundlings() {
    return sellerActivePromoBundlings;
  }

  public void setSellerActivePromoBundlings(Set<String> sellerActivePromoBundlings) {
    this.sellerActivePromoBundlings = sellerActivePromoBundlings;
  }

  @Override
  public Long getVersion() {
    return version;
  }

  @Override
  public void setVersion(Long version) {
    this.version = version;
  }

  public boolean isCncActive() {
    return cncActive;
  }

  public void setCncActive(boolean cncActive) {
    this.cncActive = cncActive;
  }

  public boolean isFreeSample() {return freeSample;}

  public boolean isItemPickUpPointMarkForDelete() {return itemPickUpPointMarkForDelete;}

  public String getMasterSku() {
    return masterSku;
  }

  public void setMasterSku(String masterSku) {
    this.masterSku = masterSku;
  }

  public boolean isSubscribableAtL5Level() {
    return subscribableAtL5Level;
  }

  public void setSubscribableAtL5Level(boolean subscribableAtL5Level) {
    this.subscribableAtL5Level = subscribableAtL5Level;
  }
}
