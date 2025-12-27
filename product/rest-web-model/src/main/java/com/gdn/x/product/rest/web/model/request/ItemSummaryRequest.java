package com.gdn.x.product.rest.web.model.request;

import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.enums.ChannelName;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * It's model to filter product item level 3 summary from X-Product.
 * String merchantSku is used to filter based on single value.
 * List<String> merchantSkus is used to filter based on multiple value. 
 * merchantSku will automatically added into merchantSkus and it will do filter by array data.
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSummaryRequest extends BaseRequest {
  private static final long serialVersionUID = 1L;

  private String merchantCode;
  private String merchantSku;
  private String productItemName;
  private List<String> itemSkus;
  private String itemSkuKeyword;
  private String masterCategoryCode;
  private String salesCategoryCode;
  private Double offerPrice;
  private String pickupPointCode;
  private Boolean discoverable;
  private Boolean buyable;
  private ChannelName channelName;
  private String productCode;
  private String itemCode;
  private Boolean isArchived;
  private Boolean isTradingProduct;
  private boolean cncActivated;
  private List<String> merchantSkus;
  private List<String> categoryCodes;
  private List<String> productSkus;
  private List<String> excludedItemSkus;
  private List<String> pickupPointCodes;
  private String searchKey;
  private Boolean isSuspended;
  private String linkedPartnerCode;
  private Boolean markForDelete;
  private boolean fbbActivated;

  public ItemSummaryRequest() {}

  public ItemSummaryRequest(String merchantCode, String productItemName, List<String> itemSkus,
      String itemSkuKeyword, String masterCategoryCode, Double offerPrice, String pickupPointCode,
      Boolean discoverable, Boolean buyable, ChannelName channelName, String itemCode) {
    super();
    this.merchantCode = merchantCode;
    this.productItemName = productItemName;
    this.itemSkus = itemSkus;
    this.itemSkuKeyword = itemSkuKeyword;
    this.masterCategoryCode = masterCategoryCode;
    this.offerPrice = offerPrice;
    this.pickupPointCode = pickupPointCode;
    this.discoverable = discoverable;
    this.buyable = buyable;
    this.channelName = channelName;
    this.itemCode = itemCode;
  }

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }
  
  public List<String> getMerchantSkus() {
    return merchantSkus;
  }

  public void setMerchantSkus(List<String> merchantSkus) {
    this.merchantSkus = merchantSkus;
  }

  public Boolean getBuyable() {
    return this.buyable;
  }


  public ChannelName getChannelName() {
    return this.channelName;
  }


  public Boolean getDiscoverable() {
    return this.discoverable;
  }

  public Boolean getIsTradingProduct() {
    return this.isTradingProduct;
  }

  public String getItemCode() {
    return this.itemCode;
  }

  public String getItemSkuKeyword() {
    return this.itemSkuKeyword;
  }

  public List<String> getItemSkus() {
    return this.itemSkus;
  }

  public String getMasterCategoryCode() {
    return this.masterCategoryCode;
  }

  public String getMerchantCode() {
    return this.merchantCode;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public Double getOfferPrice() {
    return this.offerPrice;
  }

  public String getPickupPointCode() {
    return this.pickupPointCode;
  }

  public String getProductItemName() {
    return this.productItemName;
  }

  public String getSalesCategoryCode() {
    return this.salesCategoryCode;
  }

  public Boolean getArchived() { return isArchived; }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public void setChannelName(ChannelName channelName) {
    this.channelName = channelName;
  }

  public void setDiscoverable(Boolean discoverable) {
    this.discoverable = discoverable;
  }

  public void setIsTradingProduct(Boolean isTradingProduct) {
    this.isTradingProduct = isTradingProduct;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setItemSkuKeyword(String itemSku) {
    this.itemSkuKeyword = itemSku;
  }

  public void setItemSkus(List<String> itemSkus) {
    this.itemSkus = itemSkus;
  }

  /**
   * use instead {@link ItemSummaryRequest#setCategoryCodes(List)}
   *
   * @param category
   */
  @Deprecated
  public void setMasterCategoryCode(String category) {
    this.masterCategoryCode = category;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public void setOfferPrice(Double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public void setProductItemName(String productItemName) {
    this.productItemName = productItemName;
  }

  public void setSalesCategoryCode(String salesCategoryCode) {
    this.salesCategoryCode = salesCategoryCode;
  }

  public void setArchived(Boolean archived) { this.isArchived = archived; }

  public boolean isCncActivated() {
    return cncActivated;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  public List<String> getCategoryCodes() {
    return categoryCodes;
  }

  public void setCategoryCodes(List<String> categoryCodes) {
    this.categoryCodes = categoryCodes;
  }

  public List<String> getProductSkus() {
    return productSkus;
  }

  public void setProductSkus(List<String> productSkus) {
    this.productSkus = productSkus;
  }

  public List<String> getExcludedItemSkus() {
    return excludedItemSkus;
  }

  public void setExcludedItemSkus(List<String> excludedItemSkus) {
    this.excludedItemSkus = excludedItemSkus;
  }

  public String getLinkedPartnerCode() {
    return linkedPartnerCode;
  }

  public void setLinkedPartnerCode(String linkedPartnerCode) {
    this.linkedPartnerCode = linkedPartnerCode;
  }

  public List<String> getPickupPointCodes() {
    return pickupPointCodes;
  }

  public void setPickupPointCodes(List<String> pickupPointCodes) {
    this.pickupPointCodes = pickupPointCodes;
  }

  public String getSearchKey() {
    return searchKey;
  }

  public void setSearchKey(String searchKey) {
    this.searchKey = searchKey;
  }

  public Boolean getSuspended() {
    return isSuspended;
  }

  public void setSuspended(Boolean suspended) {
    isSuspended = suspended;
  }

  public Boolean getMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(Boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public boolean getFbbActivated() {
    return fbbActivated;
  }

  public void setFbbActivated(boolean fbbActivated) {
    this.fbbActivated = fbbActivated;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
      .append("merchantCode", merchantCode)
      .append("merchantSku", merchantSku)
      .append("productItemName", productItemName)
      .append("itemSkus", itemSkus)
      .append("itemSkuKeyword", itemSkuKeyword)
      .append("masterCategoryCode", masterCategoryCode)
      .append("salesCategoryCode", salesCategoryCode)
      .append("offerPrice", offerPrice)
      .append("pickupPointCode", pickupPointCode)
      .append("discoverable", discoverable)
      .append("buyable", buyable)
      .append("channelName", channelName)
      .append("productCode", productCode)
      .append("itemCode", itemCode)
      .append("isArchived", isArchived)
      .append("isTradingProduct", isTradingProduct)
      .append("cncActivated", cncActivated)
      .append("merchantSkus", merchantSkus)
      .append("categoryCodes", categoryCodes)
      .append("productSkus", productSkus)
      .append("excludedItemSkus", excludedItemSkus)
      .append("linkedPartnerCode", linkedPartnerCode)
      .append("pickupPointCodes", pickupPointCodes)
      .append("searchKey", searchKey)
      .append("isSuspended", isSuspended)
      .append("isSuspended", fbbActivated)
      .toString();
  }

}
