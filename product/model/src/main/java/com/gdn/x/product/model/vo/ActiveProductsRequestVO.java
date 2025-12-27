package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.List;

import com.gdn.common.base.GdnObjects;

public class ActiveProductsRequestVO implements Serializable {
  private static final long serialVersionUID = -5639165939053601385L;
  private String merchantCode;
  private List<String> categoryCodes;
  private List<String> pickupPointCodes;
  private Boolean discoverable;
  private Boolean buyable;
  private String searchKey;
  private String status;
  private String sortType;
  private String nameKey;
  private Boolean cncActivated;
  private Boolean tradingProduct;
  private Boolean bundleProduct;

  public Boolean getTradingProduct() {
    return tradingProduct;
  }

  public void setTradingProduct(Boolean tradingProduct) {
    this.tradingProduct = tradingProduct;
  }


  public Boolean getBundleProduct() {
    return bundleProduct;
  }

  public void setBundleProduct(Boolean bundleProduct) {
    this.bundleProduct = bundleProduct;
  }

  public String getSearchKey() {
    return searchKey;
  }

  public void setSearchKey(String searchKey) {
    this.searchKey = searchKey;
  }

  public String getMerchantCode() {
    return merchantCode;
  }

  public void setMerchantCode(String merchantCode) {
    this.merchantCode = merchantCode;
  }

  public List<String> getCategoryCodes() {
    return categoryCodes;
  }

  public void setCategoryCodes(List<String> categoryCodes) {
    this.categoryCodes = categoryCodes;
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

  public String getStatus() {return status; }

  public void setStatus(String status){ this.status = status; }

  public String getSortType() { return sortType; }

  public void setSortType(String sortType) { this.sortType = sortType; }

  public String getNameKey() { return nameKey; }

  public void setNameKey(String nameKey) { this.nameKey = nameKey; }

  public List<String> getPickupPointCodes() { return pickupPointCodes; }

  public void setPickupPointCodes(List<String> pickupPointCodes) {
    this.pickupPointCodes = pickupPointCodes;
  }

  public Boolean getCncActivated() { return cncActivated; }

  public void setCncActivated(Boolean cncActivated) {
    this.cncActivated = cncActivated;
  }

  @Override
  public boolean equals(Object o) {
    return GdnObjects.equals(this, o);
  }

  @Override
  public int hashCode() {

    return GdnObjects.hashCode(this);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("OfficialStoreRequestVO{");
    sb.append("merchantCode='").append(merchantCode).append('\'');
    sb.append(", categoryCodes='").append(categoryCodes).append('\'');
    sb.append(", discoverable=").append(discoverable).append('\'');
    sb.append(", buyable=").append(buyable).append('\'');
    sb.append(", searchKey=").append(searchKey).append('\'');
    sb.append(", status=").append(status).append('\'');
    sb.append(", sortType=").append(sortType);
    sb.append(", nameKey=").append(nameKey);
    sb.append(", pickupPointCodes=").append(pickupPointCodes);
    sb.append(", cncActivated=").append(cncActivated);
    sb.append(", tradingProduct=").append(tradingProduct);
    sb.append(", bundleProduct=").append(bundleProduct);
    sb.append('}');
    return sb.toString();
  }
}

