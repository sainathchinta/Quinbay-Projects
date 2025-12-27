package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductLevel3SummaryRequest implements Serializable {

  private static final long serialVersionUID = -4988622956530942857L;
  private String itemName;
  private String itemCode;

  /**
   * please, use {@link ProductLevel3SummaryRequest#itemSkus} instead
   */
  @Deprecated
  private String gdnSku;
  private String categoryCode;
  private Double salePrice;
  private String pickupPointCode;
  private Integer stock;
  private Boolean displayable;
  private Boolean buyable;
  private Boolean isArchived = Boolean.FALSE;
  private ProductLevel3InventoryCriteria inventoryFilter;
  private List<String> merchantSkus;
  private List<String> itemSkus;
  private List<String> categoryCodes;
  private List<String> pickupPointCodes;
  private List<String> promoTypes;
  private String searchKey;
  private List<String> productSkuList;
  private boolean ignoreArchive;

  public ProductLevel3SummaryRequest(String itemName, String gdnSku, String categoryCode,
      Double salePrice, String pickupPointCode, Integer stock) {
    super();
    this.itemName = itemName;
    this.gdnSku = gdnSku;
    this.categoryCode = categoryCode;
    this.salePrice = salePrice;
    this.pickupPointCode = pickupPointCode;
    this.stock = stock;
  }
  
  public List<String> getMerchantSkus() {
    return merchantSkus;
  }

  public void setMerchantSkus(List<String> merchantSkus) {
    this.merchantSkus = merchantSkus;
  }

  public String getItemName() {
    return itemName;
  }

  public void setItemName(String itemName) {
    this.itemName = itemName;
  }

  /**
   * use {@link ProductLevel3SummaryRequest#itemSkus}
   */
  @Deprecated
  public String getGdnSku() {
    return gdnSku;
  }

  /**
   * use {@link ProductLevel3SummaryRequest#itemSkus}
   */
  @Deprecated
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


  public Boolean getArchived() {
    return isArchived;
  }

  public void setArchived(Boolean archived) {
    this.isArchived = archived;
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

  public void setIgnoreArchive(boolean ignoreArchive) {
    this.ignoreArchive = ignoreArchive;
  }

  public boolean isIgnoreArchive() {
    return ignoreArchive;
  }

  public List<String> getPromoTypes() {
    return promoTypes;
  }

  public void setPromoTypes(List<String> promoTypes) {
    this.promoTypes = promoTypes;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("itemName", itemName).append("gdnSku", gdnSku)
        .append("categoryCode", categoryCode).append("salePrice", salePrice)
        .append("pickupPointCode", pickupPointCode).append("stock", stock)
        .append("displayable", displayable).append("buyable", buyable)
        .append("archived", isArchived).append("itemCode", itemCode).append("itemSkus", itemSkus)
        .append("inventoryFilter", inventoryFilter).append("categoryCodes", categoryCodes)
        .append("pickupPointCodes", pickupPointCodes).append("searchKey", searchKey)
        .append("productSkuList", productSkuList).append("ignoreArchive", ignoreArchive)
        .toString();
  }
}
