package com.gdn.mta.product.valueobject;

import java.util.List;

import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import org.apache.commons.lang3.builder.ToStringBuilder;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductLevel3SummaryFilter {
  private String storeId;
  private String businessPartnerCode;
  private String itemName;
  private String itemCode;

  /**
   * please, use {@link ProductLevel3SummaryFilter#itemSkus} instead
   */
  @Deprecated
  private String gdnSku;
  private String categoryCode;
  private Double salePrice;
  private String pickupPointCode;
  private List<String> promoTypes;
  private Boolean displayable;
  private Boolean buyable;
  private Integer stock;
  private ProductLevel3InventoryCriteria inventoryFilter;
  private Boolean archived;
  private List<String> merchantSkus;
  private List<String> itemSkus;
  private List<String> excludedItemSkus;
  private String linkedPartnerCode;
  private List<String> categoryCodes;
  private List<String> pickupPointCodes;
  private String searchKey;
  private List<String> productSkuList;

  public String getStoreId() {
    return storeId;
  }
  
  public List<String> getMerchantSkus() {
    return merchantSkus;
  }

  public void setMerchantSkus(List<String> merchantSkus) {
    this.merchantSkus = merchantSkus;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getItemName() {
    return itemName;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  public String getGdnSku() {
    return gdnSku;
  }

  public void setGdnSku(String gdnSku) {
    this.gdnSku = gdnSku;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public Double getSalePrice() {
    return salePrice;
  }

  public void setSalePrice(Double salePrice) {
    this.salePrice = salePrice;
  }

  public String getPickupPointCode() {
    return pickupPointCode;
  }

  public void setPickupPointCode(String pickupPointCode) {
    this.pickupPointCode = pickupPointCode;
  }

  public Boolean getDisplayable() {
    return displayable;
  }

  public void setDisplayable(Boolean displayable) {
    this.displayable = displayable;
  }

  public Boolean getBuyable() {
    return buyable;
  }

  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public Integer getStock() {
    return stock;
  }

  public void setStock(Integer stock) {
    this.stock = stock;
  }

  public ProductLevel3InventoryCriteria getInventoryFilter() {
    return inventoryFilter;
  }

  public void setInventoryFilter(ProductLevel3InventoryCriteria inventoryFilter) {
    this.inventoryFilter = inventoryFilter;
  }

  public Boolean getArchived() {
    return archived;
  }

  public void setArchived(Boolean archived) {
    this.archived = archived;
  }

  public String getItemCode() {
    return itemCode;
  }

  public void setItemCode(String itemCode) {
    this.itemCode = itemCode;
  }

  public List<String> getItemSkus() {
    return itemSkus;
  }

  public void setItemSkus(List<String> itemSkus) {
    this.itemSkus = itemSkus;
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

  public List<String> getCategoryCodes() {
    return categoryCodes;
  }

  public void setCategoryCodes(List<String> categoryCodes) {
    this.categoryCodes = categoryCodes;
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

  public List<String> getProductSkuList() {
    return productSkuList;
  }

  public void setProductSkuList(List<String> productSkuList) {
    this.productSkuList = productSkuList;
  }

  public List<String> getPromoTypes() {
    return promoTypes;
  }

  public void setPromoTypes(List<String> promoTypes) {
    this.promoTypes = promoTypes;
  }


  @Override
  public String toString() {
    return new ToStringBuilder(this)
      .append("storeId", storeId)
      .append("businessPartnerCode", businessPartnerCode)
      .append("itemName", itemName)
      .append("itemCode", itemCode)
      .append("gdnSku", gdnSku)
      .append("categoryCode", categoryCode)
      .append("salePrice", salePrice)
      .append("pickupPointCode", pickupPointCode)
      .append("displayable", displayable)
      .append("buyable", buyable)
      .append("stock", stock)
      .append("inventoryFilter", inventoryFilter)
      .append("archived", archived)
      .append("merchantSkus", merchantSkus)
      .append("itemSkus", itemSkus)
      .append("excludedItemSkus", excludedItemSkus)
      .append("categoryCodes", categoryCodes)
      .append("pickupPointCodes", pickupPointCodes)
      .append("searchKey", searchKey)
      .append("productSkuList", productSkuList)
      .toString();
  }

}
