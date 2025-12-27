package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import org.apache.commons.lang3.builder.ToStringBuilder;

@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkItemSummaryRequestVo implements Serializable {

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
  private String channelName;
  private String productCode;
  private List<String> itemCodes;
  private List<String> pristineIds;
  private Boolean isArchived;
  private Boolean isTradingProduct;
  private boolean cncActivated;
  private List<String> merchantSkus;
  private List<String> excludedItemSkus;
  private String linkedPartnerCode;

  public BulkItemSummaryRequestVo() {
  }

  public BulkItemSummaryRequestVo(String merchantCode, String productItemName,
    List<String> itemSkus, String itemSkuKeyword, String masterCategoryCode, Double offerPrice,
    String pickupPointCode, Boolean discoverable, Boolean buyable, String channelName,
    List<String> itemCodes) {
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
    this.itemCodes = itemCodes;
  }

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public String getMerchantSku() {
    return merchantSku;
  }

  public void setMerchantSku(String merchantSku) {
    this.merchantSku = merchantSku;
  }

  public String getProductItemName() {
    return productItemName;
  }

  public void setProductItemName(String productItemName) {
    this.productItemName = productItemName;
  }

  public List<String> getItemSkus() {
    return itemSkus;
  }

  public void setItemSkus(List<String> itemSkus) {
    this.itemSkus = itemSkus;
  }

  public String getItemSkuKeyword() {
    return itemSkuKeyword;
  }

  public void setItemSkuKeyword(String itemSkuKeyword) {
    this.itemSkuKeyword = itemSkuKeyword;
  }

  public String getMasterCategoryCode() {
    return masterCategoryCode;
  }

  public void setMasterCategoryCode(String masterCategoryCode) {
    this.masterCategoryCode = masterCategoryCode;
  }

  public String getSalesCategoryCode() {
    return salesCategoryCode;
  }

  public void setSalesCategoryCode(String salesCategoryCode) {
    this.salesCategoryCode = salesCategoryCode;
  }

  public Double getOfferPrice() {
    return offerPrice;
  }

  public void setOfferPrice(Double offerPrice) {
    this.offerPrice = offerPrice;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Boolean getDiscoverable() {
    return discoverable;
  }

  public void setDiscoverable(Boolean discoverable) {
    this.discoverable = discoverable;
  }

  public Boolean getBuyable() {
    return buyable;
  }

  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public String getChannelName() {
    return channelName;
  }

  public void setChannelName(String channelName) {
    this.channelName = channelName;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public List<String> getItemCodes() {
    return itemCodes;
  }

  public void setItemCodes(List<String> itemCodes) {
    this.itemCodes = itemCodes;
  }

  public Boolean getArchived() {
    return isArchived;
  }

  public void setArchived(Boolean archived) {
    isArchived = archived;
  }

  public Boolean getIsTradingProduct() {
    return isTradingProduct;
  }

  public void setIsTradingProduct(Boolean tradingProduct) {
    isTradingProduct = tradingProduct;
  }

  public boolean isCncActivated() {
    return cncActivated;
  }

  public void setCncActivated(boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  public List<String> getMerchantSkus() {
    return merchantSkus;
  }

  public void setMerchantSkus(List<String> merchantSkus) {
    this.merchantSkus = merchantSkus;
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

  public List<String> getPristineIds() {
    return pristineIds;
  }

  public void setPristineIds(List<String> pristineIds) {
    this.pristineIds = pristineIds;
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
      .append("itemCodes", itemCodes)
      .append("isArchived", isArchived)
      .append("isTradingProduct", isTradingProduct)
      .append("cncActivated", cncActivated)
      .append("merchantSkus", merchantSkus)
      .append("excludedItemSkus", excludedItemSkus)
      .append("linkedPartnerCode", linkedPartnerCode)
      .append("pristineIds", pristineIds)
      .toString();
  }
}
